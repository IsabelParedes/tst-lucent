const WASM_R_HOME = "/R_HOME";

// Build-time-only files; set LD_LIBRARY_PATH via ENV instead of mounting ldpaths.
const VFS_SKIP = new Set([`${WASM_R_HOME}/etc/ldpaths`, `${WASM_R_HOME}/etc/Makeconf`]);

const glueUrl = new URL("R", import.meta.url);
const wasmUrl = new URL("R.wasm", import.meta.url);
const rHomeUrl = new URL("R_HOME/", import.meta.url);
const rLibUrl = new URL("R_HOME/lib/", import.meta.url);
const manifestUrl = new URL("R_HOME-manifest.json", import.meta.url);

/** @type {Map<string, Uint8Array>} */
const fileCache = new Map();
/** @type {Promise<any> | null} */
let rModulePromise = null;

function rEnv() {
  return {
    R_HOME: WASM_R_HOME,
    R_DEFAULT_PACKAGES: "NULL",
    R_LIBS: `${WASM_R_HOME}/library`,
    R_LIBS_USER: "NULL",
    R_LIBS_SITE: "NULL",
    LD_LIBRARY_PATH: `${WASM_R_HOME}/lib:/lib`,
  };
}

function dstToFetchUrl(dst) {
  const prefix = `${WASM_R_HOME}/`;
  if (dst.startsWith(prefix)) {
    return new URL(dst.slice(prefix.length), rHomeUrl).href;
  }
  if (dst.startsWith("/lib/")) {
    return new URL(dst.slice(5), rLibUrl).href;
  }
  throw new Error(`Unknown mount path: ${dst}`);
}

async function mountRHome() {
  if (fileCache.size > 0) {
    return;
  }

  const res = await fetch(manifestUrl);
  if (!res.ok) {
    throw new Error(`Failed to fetch ${manifestUrl.href}: HTTP ${res.status}`);
  }
  const { files } = await res.json();
  console.info("[runApp] Mounting", files.length, "files from manifest");

  let next = 0;
  await Promise.all(
    Array.from({ length: 32 }, async () => {
      while (next < files.length) {
        const dst = files[next++];
        if (VFS_SKIP.has(dst)) {
          continue;
        }
        const fileRes = await fetch(dstToFetchUrl(dst));
        if (!fileRes.ok) {
          throw new Error(`Failed to fetch ${dstToFetchUrl(dst)}: HTTP ${fileRes.status}`);
        }
        fileCache.set(dst, new Uint8Array(await fileRes.arrayBuffer()));
      }
    })
  );

  console.info("[runApp] Cached", fileCache.size, "files");
}

function writeCachedTree(module) {
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

function verifyMountedTree(module) {
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

function locateFile(file) {
  const base = file.split("/").pop();
  if (base.endsWith(".wasm")) {
    return new URL("R.wasm", import.meta.url).href;
  }
  // Package dynlib path, e.g. /R_HOME/library/methods/libs/methods.so
  const pkgMatch = file.match(/\/library\/([^/]+)\/libs\/([^/]+)$/);
  if (pkgMatch) {
    return new URL(`R_HOME/library/${pkgMatch[1]}/libs/${pkgMatch[2]}`, import.meta.url).href;
  }
  // Core R shared libs (libR.so, libRblas.so, …)
  return new URL(`R_HOME/lib/${base}`, import.meta.url).href;
}

async function loadGlue() {
  let glue = await fetch(glueUrl).then((res) => {
    if (!res.ok) {
      throw new Error(`Failed to fetch ${glueUrl.href}: HTTP ${res.status}`);
    }
    return res.text();
  });

  const env = rEnv();
  const envLiteral = JSON.stringify(env);
  glue = glue.replace("var ENV={};", `var ENV=${envLiteral};`);
  glue = glue.replace(
    "var Module=typeof Module!=\"undefined\"?Module:{};",
    "var Module=globalThis.Module;"
  );
  glue = glue.replace(
    'env={USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};',
    `env={R_HOME:"${env.R_HOME}",R_LIBS:"${env.R_LIBS}",R_LIBS_USER:"${env.R_LIBS_USER}",R_LIBS_SITE:"${env.R_LIBS_SITE}",LD_LIBRARY_PATH:"${env.LD_LIBRARY_PATH}",USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};`
  );
  glue = glue.replace(
    'Module["callMain"]=callMain;',
    `Module["loadDynamicLibraryAsync"]=(name)=>loadDynamicLibrary(name,{loadAsync:true,global:true,nodelete:true,allowUndefined:true});
Module["callMain"]=callMain;`
  );
  return glue;
}

async function preloadWasmSideModules(module) {
  const libR = `${WASM_R_HOME}/lib/libR.so`;
  const paths = [...fileCache.keys()].filter((p) => p.endsWith(".so"));
  const ordered = [
    ...paths.filter((p) => p === libR),
    ...paths.filter((p) => p !== libR).sort(),
  ];
  console.info("[runApp] Preloading", ordered.length, "WASM side modules");
  for (const path of ordered) {
    await module.loadDynamicLibraryAsync(path);
  }
}

async function initRModule() {
  await mountRHome();

  const [glue, wasmBinary] = await Promise.all([
    loadGlue(),
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
      wasmBinary,
      locateFile,
      ENV: rEnv(),
      preRun: [() => writeCachedTree(globalThis.Module)],
      onRuntimeInitialized() {
        // Re-mount after FS.init() (runs in initRuntime between preRun and here).
        writeCachedTree(globalThis.Module);
        try {
          verifyMountedTree(globalThis.Module);
        } catch (err) {
          reject(err);
          return;
        }
        // dlopen() sync-compiles side modules; libR.so is >8MB and fails on the
        // main thread unless we async-instantiate everything first.
        preloadWasmSideModules(globalThis.Module)
          .then(() => resolve(globalThis.Module))
          .catch(reject);
      },
      onAbort(reason) {
        reject(new Error(`R.wasm aborted: ${reason}`));
      },
      print(text) {
        console.log(String(text));
      },
      printErr(text) {
        console.error(String(text));
      },
    };

    try {
      globalThis.eval(glue);
    } catch (err) {
      reject(err);
    }
  });
}

async function ensureRModule() {
  if (!rModulePromise) {
    rModulePromise = initRModule();
  }
  return rModulePromise;
}

export async function runApp(code) {
  const trimmed = code.trim();
  if (!trimmed) {
    console.warn("[runApp] No R code to run");
    return;
  }

  const Module = await ensureRModule();
  // Avoid --vanilla so /R_HOME/etc/Rprofile.site runs before default packages
  // (needed to bind methods.so native symbols such as C_R_initMethodDispatch).
  const rArgs = ["--no-restore", "--no-save", "-e", trimmed];
  console.info("[runApp] callMain", rArgs.slice(0, 3).concat(["-e", code]));
  const status = Module.callMain(rArgs);
  console.info("[runApp] callMain finished with status", status);
  return status;
}

document.getElementById("run-button")?.addEventListener("click", async () => {
  const button = document.getElementById("run-button");
  const code = document.getElementById("app-code")?.value ?? "";

  button.disabled = true;
  try {
    await runApp(code);
  } catch (err) {
    console.error("[runApp] Failed:", err);
  } finally {
    button.disabled = false;
  }
});
