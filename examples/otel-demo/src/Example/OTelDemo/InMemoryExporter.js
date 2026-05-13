import * as api from "@opentelemetry/api";
import {
  BasicTracerProvider,
  InMemorySpanExporter,
  SimpleSpanProcessor,
} from "@opentelemetry/sdk-trace-base";

const STATUS_NAMES = {
  [api.SpanStatusCode.UNSET]: "UNSET",
  [api.SpanStatusCode.OK]: "OK",
  [api.SpanStatusCode.ERROR]: "ERROR",
};

let exporter = null;

export const installInMemoryExporter = (serviceName) => () => {
  exporter = new InMemorySpanExporter();
  const provider = new BasicTracerProvider({
    spanProcessors: [new SimpleSpanProcessor(exporter)],
  });
  api.trace.setGlobalTracerProvider(provider);
};

export const readExportedSpans = () => {
  if (!exporter) return [];
  const raw = exporter.getFinishedSpans();
  return raw.map((s) => {
    const attrEntries = Object.entries(s.attributes || {});
    const parentId =
      s.parentSpanContext?.spanId || s.parentSpanId || "";
    return {
      name: s.name,
      parentId,
      status: STATUS_NAMES[s.status?.code] || "UNSET",
      attributes: attrEntries.map(([key, value]) => ({
        key,
        value: String(value),
      })),
    };
  });
};
