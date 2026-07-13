/**
 * Node repro of Lucent R.wasm bootstrap.
 * Usage: node site/scripts/lucent-r-bootstrap-test.cjs [lucent|lucent-initr|check-shiny-patch|...]
 */
const fsp = require("node:fs");
const path = require("node:path");

const siteDir = path.resolve(__dirname, "..");
const prefix = process.env.PREFIX ?? path.join(siteDir, "..", "_env-wasm");
const hostRHome = process.env.R_HOME_TREE ?? path.join(prefix, "lib", "R");
const rExecDir = path.join(hostRHome, "bin", "exec");
const rLibDir = path.join(hostRHome, "lib");
const prefixLibDir = path.join(prefix, "lib");
const mode = process.argv[2] ?? "lucent";
const wasmRHome = mode === "rtester" ? "/R" : "/R_HOME";

function createLocateFile() {
  return (file) => {
    const base = path.basename(file);
    for (const dir of [prefixLibDir, rExecDir, rLibDir]) {
      const candidate = path.join(dir, base);
      if (fsp.existsSync(candidate)) {
        return candidate;
      }
    }
    return path.join(rExecDir, base);
  };
}

function injectRWasmEvalGlue(glue) {
  const patch = `function shinyForgeResolveR(symName){
  var resolved=resolveGlobalSymbol(symName).sym;
  if(!resolved){throw new Error("R symbol not available: "+symName)}
  return resolved;
}
function shinyForgeRData(symName){
  var sym=shinyForgeResolveR(symName);
  if(typeof sym==="function"){sym=sym()}
  if(sym&&typeof sym==="object"&&"value"in sym){return sym.value}
  return getValue(sym,"i32");
}
function shinyForgeGlobalEnv(){
  var env=shinyForgeRData("R_GlobalEnv");
  var TYPEOF=shinyForgeResolveR("TYPEOF");
  if(TYPEOF(env)!==4){
    throw new Error("R_GlobalEnv is not an environment (typeof="+TYPEOF(env)+")");
  }
  return env;
}
function shinyForgeArgv(args){
  args.unshift(thisProgram);
  var argc=args.length;
  var argv=stackAlloc((argc+1)*4);
  var argv_ptr=argv;
  args.forEach(function(arg){HEAPU32[argv_ptr>>2]=stringToUTF8OnStack(arg);argv_ptr+=4});
  HEAPU32[argv_ptr>>2]=0;
  return {argc:argc,argv:argv};
}
var shinyForgeRInitialized=false;
function shinyForgeInitR(args=[]){
  if(shinyForgeRInitialized){return 0}
  var initR=shinyForgeResolveR("Rf_initialize_R");
  var av=shinyForgeArgv(args);
  var status;
  try{status=initR(av.argc,av.argv)}catch(e){return handleException(e)}
  shinyForgeResolveR("setup_Rmainloop")();
  shinyForgeRInitialized=true;
  return status;
}
function shinyForgeCallMain(args=[]){
  var entryFunction=resolveGlobalSymbol("main").sym;
  if(!entryFunction)return;
  var av=shinyForgeArgv(args);
  try{return entryFunction(av.argc,av.argv)}catch(e){return handleException(e)}
}
Module.initR=shinyForgeInitR;
Module.evalR=function(code){
  var Rf_allocVector=shinyForgeResolveR("Rf_allocVector");
  var SET_STRING_ELT=shinyForgeResolveR("SET_STRING_ELT");
  var Rf_mkCharCE=shinyForgeResolveR("Rf_mkCharCE");
  var R_ParseVector=shinyForgeResolveR("R_ParseVector");
  var Rf_length=shinyForgeResolveR("Rf_length");
  var VECTOR_ELT=shinyForgeResolveR("VECTOR_ELT");
  var Rf_eval=shinyForgeResolveR("Rf_eval");
  var R_tryEval=shinyForgeResolveR("R_tryEval");
  var Rf_protect=shinyForgeResolveR("Rf_protect");
  var Rf_unprotect=shinyForgeResolveR("Rf_unprotect");
  var Rf_asChar=shinyForgeResolveR("Rf_asChar");
  var env=shinyForgeGlobalEnv();
  var nil=shinyForgeRData("R_NilValue");
  var STRSXP=16;
  var PARSE_OK=1;
  var CE_UTF8=1;
  var charsxp=Rf_mkCharCE(stringToUTF8OnStack(code),CE_UTF8);
  var srcVec=Rf_allocVector(STRSXP,1);
  SET_STRING_ELT(srcVec,0,charsxp);
  var statusPtr=stackAlloc(4);
  setValue(statusPtr,0,"i32");
  var parsed=Rf_protect(R_ParseVector(srcVec,-1,statusPtr,nil));
  var status=getValue(statusPtr,"i32");
  if(status!==PARSE_OK){
    Rf_unprotect(1);
    throw new Error("R parse error (status "+status+")");
  }
  var n=Rf_length(parsed);
  var result=nil;
  var errorOccurredPtr=stackAlloc(4);
  for(var i=0;i<n;i++){
    setValue(errorOccurredPtr,0,"i32");
    result=R_tryEval(VECTOR_ELT(parsed,i),env,errorOccurredPtr);
    if(getValue(errorOccurredPtr,"i32")){
      var errMsg="R evaluation error";
      try{
        var errChars=Rf_asChar(result);
        if(errChars){
          errMsg=UTF8ToString(errChars);
        }
      }catch(e){}
      Rf_unprotect(1);
      throw new Error(errMsg);
    }
  }
  Rf_unprotect(1);
  return result;
};
Module["callMain"]=shinyForgeCallMain;`;
  return glue.replace('Module["callMain"]=callMain;', patch);
}

function copyTree(module, srcRoot, dstRoot) {
  module.FS.mkdirTree(dstRoot);
  for (const entry of fsp.readdirSync(srcRoot, { withFileTypes: true })) {
    const src = path.join(srcRoot, entry.name);
    const dst = `${dstRoot}/${entry.name}`;
    if (entry.isDirectory()) {
      copyTree(module, src, dst);
    } else if (entry.isFile()) {
      module.FS.writeFile(dst, fsp.readFileSync(src));
    }
  }
}

function rEnvLucent() {
  return {
    R_HOME: wasmRHome,
    R_LIBS: `${wasmRHome}/library`,
    R_LIBS_USER: "NULL",
    R_LIBS_SITE: "NULL",
    LD_LIBRARY_PATH: `${wasmRHome}/lib:/lib`,
  };
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
ok <- all(
  any(grepl("onPlotDevice", drawBody)),
  any(grepl("plotImgSrc", drawBody)),
  any(grepl("plotImgHasSrc", resizeBody)),
  any(grepl("plotVfsCacheDir", publishBody))
)
cat("[check] shiny wasm plot patch:", ok, "\\n")
if (!ok) quit(status = 1)
`;

let glue = fsp.readFileSync(path.join(rExecDir, "R"), "utf8");
const env = mode === "rtester" ? { R_HOME: wasmRHome } : rEnvLucent();
glue = glue.replace("var ENV={};", `var ENV=${JSON.stringify(env)};`);

if (mode !== "rtester") {
  glue = glue.replace(
    'var Module=typeof Module!="undefined"?Module:{};',
    "var Module=globalThis.Module;",
  );
  glue = glue.replace(
    'env={USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};',
    `env={R_HOME:"${env.R_HOME}",R_LIBS:"${env.R_LIBS ?? ""}",R_LIBS_USER:"${env.R_LIBS_USER ?? ""}",R_LIBS_SITE:"${env.R_LIBS_SITE ?? ""}",LD_LIBRARY_PATH:"${env.LD_LIBRARY_PATH ?? ""}",USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};`,
  );
  glue = injectRWasmEvalGlue(glue);
}

var Module = {
  noInitialRun: true,
  locateFile: createLocateFile(),
  preRun: [
    () => {
      copyTree(Module, hostRHome, wasmRHome);
      if (mode === "rtester" || mode === "lucent-with-lib") {
        copyTree(Module, rLibDir, "/lib");
      }
    },
  ],
  onAbort(reason) {
    console.error("[test] abort:", reason);
  },
  onRuntimeInitialized() {
    console.log(`[test] mode=${mode} R_HOME=${wasmRHome}`);
    if (mode === "lucent-callmain") {
      const status = Module.callMain(["--no-restore", "--no-save", "-e", plotProbe]);
      console.log("[test] callMain status:", status);
    } else if (mode === "lucent-dyn-reload") {
      const status = Module.callMain(["--no-restore", "--no-save", "-e", "2+4"]);
      console.log("[test] bootstrap callMain status:", status);
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
      const status = Module.callMain(["--no-restore", "--no-save", "-e", plotProbe]);
      console.log("[test] callMain status:", status);
      Module.evalR(plotProbe);
    } else if (mode === "rtester") {
      const status = Module.callMain(["--no-restore", "--vanilla", "-e", plotProbe]);
      console.log("[test] callMain status:", status);
    } else if (mode === "lucent-initr") {
      const status = Module.initR(["--no-restore", "--no-save", "--vanilla"]);
      console.log("[test] initR status:", status);
      Module.evalR(plotProbe);
    } else if (mode === "check-shiny-patch") {
      const status = Module.initR(["--no-restore", "--no-save", "--vanilla"]);
      console.log("[test] initR status:", status);
      Module.evalR(shinyWasmPlotPatchCheck);
    } else {
      const status = Module.callMain(["--no-restore", "--no-save", "-e", "2+4"]);
      console.log("[test] bootstrap callMain status:", status);
      Module.evalR(plotProbe);
    }
  },
  print: (t) => process.stdout.write(String(t) + "\n"),
  printErr: (t) => process.stderr.write(String(t) + "\n"),
};

if (mode !== "rtester") {
  globalThis.Module = Module;
}

try {
  eval(glue);
} catch (err) {
  console.error("[test] eval failed:", err);
  process.exit(1);
}
