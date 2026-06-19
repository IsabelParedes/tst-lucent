export function runApp(code, frame) {
  console.log("Running app...", { code, frame });
}

document.getElementById("run-button")?.addEventListener("click", () => {
  const code = document.getElementById("app-code")?.value ?? "";
  const frame = document.getElementById("app-frame");
  runApp(code, frame);
});
