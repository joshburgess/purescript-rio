import * as api from "@opentelemetry/api";

export const otelGetTracer = (name) => () => api.trace.getTracer(name);

export const otelStartRootSpan = (tracer) => (name) => (attrs) => () => {
  const span = tracer.startSpan(name);
  for (let i = 0; i < attrs.length; i++) {
    span.setAttribute(attrs[i].key, attrs[i].value);
  }
  return span;
};

export const otelStartChildSpan = (tracer) => (name) => (attrs) => (parent) => () => {
  const ctx = api.trace.setSpan(api.context.active(), parent);
  const span = tracer.startSpan(name, undefined, ctx);
  for (let i = 0; i < attrs.length; i++) {
    span.setAttribute(attrs[i].key, attrs[i].value);
  }
  return span;
};

export const otelSetAttribute = (span) => (key) => (value) => () => {
  span.setAttribute(key, value);
};

export const otelEndSpan = (span) => () => {
  span.setStatus({ code: api.SpanStatusCode.OK });
  span.end();
};

export const refEq = (a) => (b) => a === b;
