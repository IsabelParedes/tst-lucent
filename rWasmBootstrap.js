import { injectRWasmEvalGlue } from "./rWasmEval.js";
import { isHttpuvDebug } from "./httpuv-debug.js";
import { runLaterWasmTests } from "./rWasmLaterTest.js";

/** @type {(() => void) | null} */
let evalRPostFlush = null;

/**
 * @param {() => void} fn
 */
export function setEvalRPostFlush(fn) {
  evalRPostFlush = fn;
}

export const WASM_R_HOME = "/R_HOME";
export const WEB_APP_DIR = "/webApp";
export const WEB_APP_R = `${WEB_APP_DIR}/app.R`;
export const HTTPUV_DEBUG_R = "/httpuvDebug.R";

const VFS_SKIP = new Set([`${WASM_R_HOME}/etc/ldpaths`, `${WASM_R_HOME}/etc/Makeconf`]);

/**
 * @param {string} moduleUrl import.meta.url of the worker (or host) loading R.wasm
 */
export function rEnv(moduleUrl) {
  const wasmRHome = WASM_R_HOME;
  return {
    R_HOME: wasmRHome,
    R_DEFAULT_PACKAGES: "NULL",
    R_LIBS: `${wasmRHome}/library`,
    R_LIBS_USER: "NULL",
    R_LIBS_SITE: "NULL",
    LD_LIBRARY_PATH: `${wasmRHome}/lib:/lib`,
  };
}

/**
 * @param {string} moduleUrl
 */
export function createAssetUrls(moduleUrl) {
  const base = new URL(".", moduleUrl);
  return {
    glue: new URL("R", base),
    wasm: new URL("R.wasm", base),
    rHome: new URL("R_HOME/", base),
    rLib: new URL("R_HOME/lib/", base),
    manifest: new URL("R_HOME-manifest.json", base),
  };
}

/**
 * @param {string} moduleUrl
 */
export function createLocateFile(moduleUrl) {
  const base = new URL(".", moduleUrl);
  return function locateFile(file) {
    const fileBase = file.split("/").pop();
    if (fileBase.endsWith(".wasm")) {
      return new URL("R.wasm", base).href;
    }
    const pkgMatch = file.match(/\/library\/([^/]+)\/libs\/([^/]+)$/);
    if (pkgMatch) {
      return new URL(`R_HOME/library/${pkgMatch[1]}/libs/${pkgMatch[2]}`, base).href;
    }
    return new URL(`R_HOME/lib/${fileBase}`, base).href;
  };
}

/**
 * @param {string} moduleUrl
 * @param {Map<string, Uint8Array>} fileCache
 */
function dstToFetchUrl(moduleUrl, dst) {
  const { rHome, rLib } = createAssetUrls(moduleUrl);
  const prefix = `${WASM_R_HOME}/`;
  if (dst.startsWith(prefix)) {
    return new URL(dst.slice(prefix.length), rHome).href;
  }
  if (dst.startsWith("/lib/")) {
    return new URL(dst.slice(5), rLib).href;
  }
  throw new Error(`Unknown mount path: ${dst}`);
}

/**
 * @param {string} moduleUrl
 * @returns {Promise<Map<string, Uint8Array>>}
 */
export async function mountRHome(moduleUrl) {
  const fileCache = new Map();
  const { manifest } = createAssetUrls(moduleUrl);

  const res = await fetch(manifest);
  if (!res.ok) {
    throw new Error(`Failed to fetch ${manifest.href}: HTTP ${res.status}`);
  }
  const { files } = await res.json();
  console.info("[rWasm] Mounting", files.length, "files from manifest");

  let next = 0;
  await Promise.all(
    Array.from({ length: 32 }, async () => {
      while (next < files.length) {
        const dst = files[next++];
        if (VFS_SKIP.has(dst)) {
          continue;
        }
        const fileRes = await fetch(dstToFetchUrl(moduleUrl, dst));
        if (!fileRes.ok) {
          throw new Error(`Failed to fetch ${dstToFetchUrl(moduleUrl, dst)}: HTTP ${fileRes.status}`);
        }
        fileCache.set(dst, new Uint8Array(await fileRes.arrayBuffer()));
      }
    }),
  );

  console.info("[rWasm] Cached", fileCache.size, "files");
  return fileCache;
}

/**
 * @param {object} module
 * @param {Map<string, Uint8Array>} fileCache
 */
export function writeCachedTree(module, fileCache) {
  for (const path of fileCache.keys()) {
    const parent = path.substring(0, path.lastIndexOf("/"));
    if (parent) {
      module.FS.mkdirTree(parent);
    }
  }
  for (const [path, data] of fileCache) {
    module.FS.writeFile(path, data);
  }
}

/**
 * @param {object} module
 */
export function verifyMountedTree(module) {
  const methodsSo = `${WASM_R_HOME}/library/methods/libs/methods.so`;
  const info = module.FS.analyzePath(methodsSo);
  if (!info.exists) {
    throw new Error(`Mounted FS is missing ${methodsSo}`);
  }
  const data = module.FS.readFile(methodsSo, { encoding: "binary" });
  if (!(data[0] === 0 && data[1] === 97 && data[2] === 115 && data[3] === 109)) {
    throw new Error(`${methodsSo} is not a wasm module (bad magic bytes)`);
  }
}

/**
 * @param {string} moduleUrl
 */
async function loadGlue(moduleUrl) {
  const { glue: glueUrl } = createAssetUrls(moduleUrl);
  let glue = await fetch(glueUrl).then((res) => {
    if (!res.ok) {
      throw new Error(`Failed to fetch ${glueUrl.href}: HTTP ${res.status}`);
    }
    return res.text();
  });

  const env = rEnv(moduleUrl);
  const envLiteral = JSON.stringify(env);
  glue = glue.replace("var ENV={};", `var ENV=${envLiteral};`);
  glue = glue.replace(
    "var Module=typeof Module!=\"undefined\"?Module:{};",
    "var Module=globalThis.Module;",
  );
  glue = glue.replace(
    'env={USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};',
    `env={R_HOME:"${env.R_HOME}",R_LIBS:"${env.R_LIBS}",R_LIBS_USER:"${env.R_LIBS_USER}",R_LIBS_SITE:"${env.R_LIBS_SITE}",LD_LIBRARY_PATH:"${env.LD_LIBRARY_PATH}",USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};`,
  );
  glue = injectRWasmEvalGlue(glue);
  return glue;
}

/**
 * @param {object} module
 * @param {Map<string, Uint8Array>} fileCache
 */
export async function preloadWasmSideModules(module, fileCache) {
  const libR = `${WASM_R_HOME}/lib/libR.so`;
  const paths = [...fileCache.keys()].filter((p) => p.endsWith(".so"));
  const ordered = [
    ...paths.filter((p) => p === libR),
    ...paths.filter((p) => p !== libR).sort(),
  ];
  console.info("[rWasm] Preloading", ordered.length, "WASM side modules");
  for (const path of ordered) {
    await module.loadDynamicLibraryAsync(path);
  }
}

/**
 * @param {object} Module
 * @param {string} code
 */
export function evalR(Module, code) {
  if (typeof Module.evalR !== "function") {
    throw new Error("Module.evalR is missing; check rWasmEval.js glue patch");
  }
  if (Module._rWasmEvalDepth > 0) {
    throw new Error("reentrant evalR");
  }
  Module._rWasmEvalDepth = 1;
  try {
    Module.evalR(code);
  } finally {
    Module._rWasmEvalDepth = 0;
    evalRPostFlush?.();
  }
}

/**
 * @param {string} moduleUrl
 * @param {object} module
 */
export async function mountHttpuvDebugScript(moduleUrl, module) {
  if (!isHttpuvDebug()) {
    return;
  }
  const url = new URL("httpuvDebug.R", new URL(".", moduleUrl));
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error(`Failed to fetch ${url.href}: HTTP ${res.status}`);
  }
  module.FS.writeFile(HTTPUV_DEBUG_R, await res.text());
  console.info("[rWasm] Mounted httpuv debug script");
}

/**
 * @param {object} Module
 * @param {string} moduleUrl
 */
export async function bootstrapRSession(Module, moduleUrl) {
  const status = Module.callMain(["--no-restore", "--no-save", "-e", "2+4"]);
  if (status !== 0) {
    throw new Error(`R bootstrap failed with status ${status}`);
  }

  await runLaterWasmTests(moduleUrl, Module, (text) => console.info(String(text)));
  await mountHttpuvDebugScript(moduleUrl, Module);

  evalR(Module, "suppressPackageStartupMessages(library(httpuv))");
  evalR(Module, 'setwd("/")');
  console.info("[rWasm] R session ready");
}

/**
 * @param {object} Module
 * @param {string} source
 */
export function writeWebAppToVfs(Module, source) {
  Module.FS.mkdirTree(WEB_APP_DIR);
  Module.FS.writeFile(WEB_APP_R, source);
  console.info("[rWasm] Wrote", WEB_APP_R, `(${source.length} bytes)`);
}

/**
 * Load and initialize R.wasm in the current global scope (main thread or worker).
 * @param {object} options
 * @param {string} options.moduleUrl import.meta.url of the loader
 * @param {object} [options.httpuv] pre-installed Module.httpuv object
 * @param {(text: string) => void} [options.print]
 * @param {(text: string) => void} [options.printErr]
 */
export async function initRModule({ moduleUrl, httpuv, print, printErr }) {
  const fileCache = await mountRHome(moduleUrl);
  const env = rEnv(moduleUrl);
  const locateFile = createLocateFile(moduleUrl);
  const { wasm: wasmUrl } = createAssetUrls(moduleUrl);

  const [glue, wasmBinary] = await Promise.all([
    loadGlue(moduleUrl),
    fetch(wasmUrl).then((res) => {
      if (!res.ok) {
        throw new Error(`Failed to fetch ${wasmUrl.href}: HTTP ${res.status}`);
      }
      return res.arrayBuffer();
    }),
  ]);

  return new Promise((resolve, reject) => {
    globalThis.Module = {
      noInitialRun: true,
      _rWasmEvalDepth: 0,
      wasmBinary,
      locateFile,
      ENV: env,
      httpuv: httpuv ?? globalThis.Module?.httpuv,
      preRun: [() => writeCachedTree(globalThis.Module, fileCache)],
      onRuntimeInitialized() {
        writeCachedTree(globalThis.Module, fileCache);
        try {
          verifyMountedTree(globalThis.Module);
        } catch (err) {
          reject(err);
          return;
        }
        preloadWasmSideModules(globalThis.Module, fileCache)
          .then(() => bootstrapRSession(globalThis.Module, moduleUrl))
          .then(() => resolve(globalThis.Module))
          .catch(reject);
      },
      onAbort(reason) {
        reject(new Error(`R.wasm aborted: ${reason}`));
      },
      print(text) {
        (print ?? console.log)(String(text));
      },
      printErr(text) {
        (printErr ?? console.error)(String(text));
      },
    };

    try {
      globalThis.eval(glue);
    } catch (err) {
      reject(err);
    }
  });
}
