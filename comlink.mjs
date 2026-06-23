/**
 * Re-export Comlink with a relative path so module workers and service workers
 * can load it (they do not inherit the document import map).
 */
export * from "./node_modules/comlink/dist/esm/comlink.mjs";
