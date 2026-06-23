/** Comlink MessagePort roles brokered by the host page between SW and R worker. */
export const COMLINK = {
  PORT_HANDOFF: "httpuv_comlink_port",
  ROLE: {
    /** R worker exposes the httpuv host API on this port. */
    R_HOST: "r_host",
    /** Service worker exposes response delivery on this port. */
    SW_DELIVERY: "sw_delivery",
  },
};
