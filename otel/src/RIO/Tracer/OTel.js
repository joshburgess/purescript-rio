import * as api from "@opentelemetry/api";

export const otelGetTracer = (name) => () => api.trace.getTracer(name);

export const otelStartRootSpan = (tracer) => (name) => () =>
  tracer.startSpan(name);

export const otelStartChildSpan = (tracer) => (name) => (parent) => () => {
  const ctx = api.trace.setSpan(api.context.active(), parent);
  return tracer.startSpan(name, undefined, ctx);
};

export const otelSetAttribute = (span) => (key) => (value) => () => {
  span.setAttribute(key, value);
};

export const otelEndSpanOk = (span) => () => {
  span.setStatus({ code: api.SpanStatusCode.OK });
  span.end();
};

export const otelEndSpanError = (span) => () => {
  span.setStatus({ code: api.SpanStatusCode.ERROR });
  span.end();
};

export const otelEndSpanInterrupted = (span) => () => {
  span.setStatus({
    code: api.SpanStatusCode.ERROR,
    message: "interrupted",
  });
  span.end();
};
