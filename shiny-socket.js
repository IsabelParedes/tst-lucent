/**
 * Virtual WebSocket for Shiny in the browser (fetch + service worker long-poll).
 * Loaded as a module in the Shiny app iframe; overrides Shiny.createSocket.
 */

const SESSION = "__session__/";

/**
 * Minimal WebSocket stand-in using the httpuv session fetch API.
 */
export class VirtualShinySocket {
  static CONNECTING = 0;
  static OPEN = 1;
  static CLOSING = 2;
  static CLOSED = 3;

  constructor() {
    /** @type {number} */
    this.readyState = VirtualShinySocket.CONNECTING;
    this.binaryType = "arraybuffer";
    /** @type {string | null} */
    this._handle = null;
    this._closed = false;
    this._recvActive = false;

    /** @type {((event: Event) => void) | null} */
    this.onopen = null;
    /** @type {((event: MessageEvent) => void) | null} */
    this.onmessage = null;
    /** @type {((event: CloseEvent) => void) | null} */
    this.onclose = null;
    /** @type {((event: Event) => void) | null} */
    this.onerror = null;

    void this._connect();
  }

  async _connect() {
    try {
      const res = await fetch(`${SESSION}open`, { method: "POST" });
      if (!res.ok) {
        throw new Error(`session open failed: HTTP ${res.status}`);
      }
      const { handle } = await res.json();
      if (!handle) {
        throw new Error("session open response missing handle");
      }
      this._handle = handle;
      this.readyState = VirtualShinySocket.OPEN;
      this._recvActive = true;
      void this._recvLoop();
      this.onopen?.(new Event("open"));
    } catch (err) {
      console.error("[shiny-socket] connect failed", err);
      this.readyState = VirtualShinySocket.CLOSED;
      this.onerror?.(new Event("error"));
      this.onclose?.(new CloseEvent("close", { code: 1006, wasClean: false }));
    }
  }

  async _recvLoop() {
    while (this._recvActive && this._handle) {
      try {
        const res = await fetch(`${SESSION}recv?handle=${encodeURIComponent(this._handle)}`);
        if (!this._recvActive) {
          return;
        }
        if (res.status === 204) {
          continue;
        }
        if (!res.ok) {
          throw new Error(`session recv failed: HTTP ${res.status}`);
        }

        const wsType = res.headers.get("X-Httpuv-WS-Type") ?? "websocket.send";
        if (wsType === "websocket.close") {
          this._finishClose(1000, "", true);
          return;
        }

        const data =
          this.binaryType === "arraybuffer"
            ? await res.arrayBuffer()
            : await res.text();
        this.onmessage?.(new MessageEvent("message", { data }));
      } catch (err) {
        if (!this._recvActive) {
          return;
        }
        console.error("[shiny-socket] recv loop error", err);
        this._finishClose(1006, String(err), false);
        return;
      }
    }
  }

  /**
   * @param {string | ArrayBuffer | ArrayBufferView} data
   */
  send(data) {
    if (this.readyState !== VirtualShinySocket.OPEN || !this._handle) {
      throw new Error("WebSocket is not open");
    }

    const isBinary = typeof data !== "string";
    const body = isBinary
      ? data instanceof ArrayBuffer
        ? data
        : data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength)
      : data;

    void fetch(`${SESSION}send?handle=${encodeURIComponent(this._handle)}`, {
      method: "POST",
      headers: isBinary ? {} : { "Content-Type": "text/plain; charset=UTF-8" },
      body,
    }).catch((err) => {
      console.error("[shiny-socket] send failed", err);
    });
  }

  /**
   * @param {number} [code]
   * @param {string} [reason]
   */
  close(code = 1000, reason = "") {
    if (this.readyState === VirtualShinySocket.CLOSED || this.readyState === VirtualShinySocket.CLOSING) {
      return;
    }
    this.readyState = VirtualShinySocket.CLOSING;
    const handle = this._handle;
    this._recvActive = false;
    if (handle) {
      void fetch(`${SESSION}close?handle=${encodeURIComponent(handle)}`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code, reason }),
      }).catch((err) => {
        console.error("[shiny-socket] close failed", err);
      });
    }
    this._finishClose(code, reason, true);
  }

  /**
   * @param {number} code
   * @param {string} reason
   * @param {boolean} wasClean
   */
  _finishClose(code, reason, wasClean) {
    this._recvActive = false;
    this.readyState = VirtualShinySocket.CLOSED;
    this.onclose?.(new CloseEvent("close", { code, reason, wasClean }));
  }
}

/**
 * Install VirtualShinySocket as Shiny.createSocket (call before Shiny connects).
 */
export function installVirtualShinySocket() {
  const factory = () => new VirtualShinySocket();

  const apply = () => {
    if (typeof globalThis.Shiny === "object" && globalThis.Shiny !== null) {
      globalThis.Shiny.createSocket = factory;
    } else {
      globalThis.Shiny = { createSocket: factory };
    }
  };

  apply();
  document.addEventListener("DOMContentLoaded", apply, { once: true });
  console.info("[shiny-socket] VirtualShinySocket installed");
}

installVirtualShinySocket();
