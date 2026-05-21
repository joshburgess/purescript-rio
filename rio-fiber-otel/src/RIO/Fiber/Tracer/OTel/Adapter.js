import * as api from "@opentelemetry/api";

export const otelGetTracer = (name) => () => api.trace.getTracer(name);

// Map the rio-fiber SpanKind tag (string) to an OTel SpanKind code.
const kindToOTel = (kind) => {
  switch (kind) {
    case "Server":
      return api.SpanKind.SERVER;
    case "Client":
      return api.SpanKind.CLIENT;
    case "Producer":
      return api.SpanKind.PRODUCER;
    case "Consumer":
      return api.SpanKind.CONSUMER;
    case "Internal":
    default:
      return api.SpanKind.INTERNAL;
  }
};

export const otelStartRootSpan = (tracer) => (name) => (attrs) => (kind) => () => {
  const span = tracer.startSpan(name, { kind: kindToOTel(kind) });
  for (let i = 0; i < attrs.length; i++) {
    span.setAttribute(attrs[i].key, attrs[i].value);
  }
  return span;
};

export const otelStartChildSpan = (tracer) => (name) => (attrs) => (kind) => (parent) => () => {
  const ctx = api.trace.setSpan(api.context.active(), parent);
  const span = tracer.startSpan(name, { kind: kindToOTel(kind) }, ctx);
  for (let i = 0; i < attrs.length; i++) {
    span.setAttribute(attrs[i].key, attrs[i].value);
  }
  return span;
};

export const otelSetAttribute = (span) => (key) => (value) => () => {
  span.setAttribute(key, value);
};

export const otelAddEvent = (span) => (name) => (attrs) => () => {
  const eventAttrs = {};
  for (let i = 0; i < attrs.length; i++) {
    eventAttrs[attrs[i].key] = attrs[i].value;
  }
  span.addEvent(name, eventAttrs);
};

export const otelAddLink = (span) => (linkTarget) => () => {
  // The OTel JS API takes links at span creation time. For
  // post-creation linking we attach the linked context as an
  // attribute so it's at least recoverable from the trace.
  const ctx = linkTarget.spanContext();
  if (ctx) {
    span.setAttribute("link.trace_id", ctx.traceId);
    span.setAttribute("link.span_id", ctx.spanId);
  }
};

export const otelSetStatusOk = (span) => () => {
  span.setStatus({ code: api.SpanStatusCode.OK });
};

export const otelSetStatusError = (span) => (msg) => () => {
  span.setStatus({ code: api.SpanStatusCode.ERROR, message: msg });
};

export const otelSetStatusUnset = (span) => () => {
  span.setStatus({ code: api.SpanStatusCode.UNSET });
};

export const otelEndSpan = (span) => () => {
  span.end();
};

export const refEq = (a) => (b) => a === b;
