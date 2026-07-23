/**
 * Node repro of Lucent Rmain bootstrap.
 * Usage: node site/scripts/lucent-r-bootstrap-test.cjs [lucent|lucent-initr|check-shiny-patch|...]
 */
const fsp = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const { pathToFileURL } = require("node:url");

const siteDir = path.resolve(__dirname, "..");
const hostPrefixDir = process.env.PREFIX ?? path.join(siteDir, "_env-wasm");
const hostRHome = process.env.R_HOME_TREE ?? path.join(hostPrefixDir, "lib", "R");
const rBinDir = path.join(hostPrefixDir, "bin");
const rExecDir = path.join(hostRHome, "bin", "exec");
const rLibDir = path.join(hostRHome, "lib");
const prefixLibDir = path.join(hostPrefixDir, "lib");
const mode = process.argv[2] ?? "lucent";
const wasmRHome = mode === "rtester" ? "/R" : "/lib/R";
const useRmain = mode !== "rtester";

const VFS_SKIP = new Set([`${wasmRHome}/etc/ldpaths`, `${wasmRHome}/etc/Makeconf`]);

function createLocateFile() {
  return (file) => {
    const fileBase = path.basename(file);
    if (fileBase.endsWith(".wasm")) {
      if (useRmain) {
        return path.join(rBinDir, "Rmain.wasm");
      }
      return path.join(rExecDir, "R.wasm");
    }
    const pkgMatch = file.match(/\/library\/([^/]+)\/libs\/([^/]+)$/);
    if (pkgMatch) {
      const candidate = path.join(hostRHome, "library", pkgMatch[1], "libs", pkgMatch[2]);
      if (fsp.existsSync(candidate)) {
        return candidate;
      }
    }
    for (const dir of [prefixLibDir, rBinDir, rExecDir, rLibDir]) {
      const candidate = path.join(dir, fileBase);
      if (fsp.existsSync(candidate)) {
        return candidate;
      }
    }
    return path.join(rLibDir, fileBase);
  };
}

function copyTree(module, srcRoot, dstRoot, skip = new Set()) {
  module.FS.mkdirTree(dstRoot);
  for (const entry of fsp.readdirSync(srcRoot, { withFileTypes: true })) {
    const src = path.join(srcRoot, entry.name);
    const dst = `${dstRoot}/${entry.name}`.replace(/\/+/g, "/");
    if (entry.isDirectory()) {
      copyTree(module, src, dst, skip);
    } else if (entry.isFile()) {
      if (skip.has(dst)) {
        continue;
      }
      module.FS.writeFile(dst, fsp.readFileSync(src));
    }
  }
}

function mountRHomeLibToSlashLib(module) {
  for (const entry of fsp.readdirSync(rLibDir, { withFileTypes: true })) {
    if (!entry.isFile() || !entry.name.endsWith(".so")) {
      continue;
    }
    module.FS.mkdirTree("/lib");
    module.FS.writeFile(`/lib/${entry.name}`, fsp.readFileSync(path.join(rLibDir, entry.name)));
  }
}

function mountPrefix(module) {
  if (mode === "rtester") {
    copyTree(module, hostRHome, wasmRHome);
    copyTree(module, rLibDir, "/lib");
    return;
  }
  copyTree(module, hostPrefixDir, "/", VFS_SKIP);
  mountRHomeLibToSlashLib(module);
}

const plotProbe = `
tryCatch({
  cat("[probe] graphics:", "graphics" %in% loadedNamespaces(), "\\n")
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 100, height = 100)
  graphics::plot.new()
  cat("[probe] plot.new() ok\\n")
  grDevices::dev.off()
}, error = function(e) cat("[probe] error:", conditionMessage(e), "\\n"))
`;

const shinyWasmPlotPatchCheck = `
suppressPackageStartupMessages(library(shiny))
drawBody <- deparse(body(getFromNamespace("drawPlot", "shiny")))
resizeBody <- deparse(body(getFromNamespace("resizeSavedPlot", "shiny")))
publishBody <- deparse(body(getFromNamespace("plotPublishPng", "shiny")))
fileUrlBody <- deparse(body(ShinySession$public_methods$fileUrl))
ok <- all(
  any(grepl("plotPublishPng", drawBody)),
  any(grepl("plotImgHasSrc", resizeBody)),
  any(grepl("wasmPublishFileUrl", publishBody)),
  any(grepl("wasmPublishFileUrl", fileUrlBody))
)
cat("[check] shiny wasm plot patch:", ok, "\\n")
if (!ok) quit(status = 1)
`;

function runProbe(Module) {
  console.log(`[test] mode=${mode} R_HOME=${wasmRHome} prefix=${hostPrefixDir}`);

  if (mode === "lucent-callmain") {
    const status = Module.callMain(["--no-restore", "--no-save", "-e", plotProbe]);
    console.log("[test] callMain status:", status);
  } else if (mode === "lucent-dyn-reload") {
    const status = Module.initR(["--no-restore", "--no-save", "--vanilla"]);
    console.log("[test] initR status:", status);
    Module.evalR(`
ns <- asNamespace("graphics")
dlls <- getNamespaceDlls(ns)
if (length(dlls)) dyn.unload(dlls[[1]][["path"]], ns)
suppressPackageStartupMessages(library(graphics))
${plotProbe}`);
  } else if (mode === "lucent-second-callmain") {
    let status = Module.callMain(["--no-restore", "--no-save", "-e", "2+4"]);
    console.log("[test] first callMain status:", status);
    status = Module.callMain(["--no-restore", "--no-save", "-e", plotProbe]);
    console.log("[test] second callMain status:", status);
  } else if (mode === "lucent-callmain-then-eval") {
    const status = Module.initR(["--no-restore", "--no-save", "--vanilla"]);
    console.log("[test] initR status:", status);
    Module.evalR(plotProbe);
  } else if (mode === "rtester") {
    const status = Module.callMain(["--no-restore", "--vanilla", "-e", plotProbe]);
    console.log("[test] callMain status:", status);
  } else if (mode === "check-shiny-patch") {
    const status = Module.initR(["--no-restore", "--no-save", "--vanilla"]);
    console.log("[test] initR status:", status);
    Module.evalR(shinyWasmPlotPatchCheck);
  } else {
    const status = Module.initR(["--no-restore", "--no-save", "--vanilla"]);
    console.log("[test] initR status:", status);
    Module.evalR(plotProbe);
  }
}

async function runRmain() {
  const gluePath = path.join(rBinDir, "Rmain.js");
  if (!fsp.existsSync(gluePath)) {
    throw new Error(
      `Missing ${gluePath}; rebuild r-main with -sEXPORT_ES6=1 (installs Rmain.js)`,
    );
  }
  const mod = await import(pathToFileURL(gluePath).href);
  const createRmain = mod.default ?? mod.Rmain;
  if (typeof createRmain !== "function") {
    throw new Error(
      "Rmain factory missing; rebuild with -sMODULARIZE=1 -sEXPORT_NAME=Rmain -sEXPORT_ES6=1",
    );
  }

  const Module = {
    noInitialRun: true,
    locateFile: createLocateFile(),
    preRun: [() => mountPrefix(Module)],
    onAbort(reason) {
      console.error("[test] abort:", reason);
    },
    print: (t) => process.stdout.write(String(t) + "\n"),
    printErr: (t) => process.stderr.write(String(t) + "\n"),
  };

  await createRmain(Module);
  runProbe(Module);
}

function runRtester() {
  const gluePath = path.join(rExecDir, "R");
  let glue = fsp.readFileSync(gluePath, "utf8");
  const envLiteral = JSON.stringify({ R_HOME: wasmRHome });
  glue = glue.replace(/var ENV = \{\s*\};/, `var ENV = ${envLiteral};`);
  glue = glue.replace(/var ENV=\{\};/, `var ENV=${envLiteral};`);

  var Module = {
    noInitialRun: true,
    locateFile: createLocateFile(),
    preRun: [() => mountPrefix(Module)],
    onAbort(reason) {
      console.error("[test] abort:", reason);
    },
    onRuntimeInitialized() {
      runProbe(Module);
    },
    print: (t) => process.stdout.write(String(t) + "\n"),
    printErr: (t) => process.stderr.write(String(t) + "\n"),
  };

  vm.runInThisContext(glue);
}

(async () => {
  try {
    if (useRmain) {
      await runRmain();
    } else {
      runRtester();
    }
  } catch (err) {
    console.error("[test] failed:", err);
    process.exit(1);
  }
})();
