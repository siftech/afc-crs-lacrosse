#!/usr/bin/env python3

import argparse
import json
import logging
from opentelemetry.exporter.otlp.proto.grpc._log_exporter import OTLPLogExporter
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.trace import Status, StatusCode, get_tracer, set_tracer_provider
from opentelemetry.sdk._logs import LoggerProvider, LoggingHandler
from opentelemetry.sdk._logs.export import BatchLogRecordProcessor, ConsoleLogExporter
from opentelemetry.sdk.resources import Resource
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor, ConsoleSpanExporter
import os

if __name__ == "__main__":
    missing_vars = [
        var
        for var in [
            "AGENT_NAME",
            "CIRCA_HOST",
            "OTEL_EXPORTER_OTLP_ENDPOINT",
            "OTEL_EXPORTER_OTLP_HEADERS",
            "OTEL_EXPORTER_OTLP_PROTOCOL",
        ]
        if var not in os.environ
    ]
    if len(missing_vars) != 0:
        print(f"Missing environment variable(s): {', '.join(missing_vars)}")
        exit(100)

    parser = argparse.ArgumentParser(description = 'send-telemetry-event')
    parser.add_argument("action_category", type=str, help="crs.action.category")
    parser.add_argument("action_name", type=str, help="crs.action.name")
    parser.add_argument("event_name", type=str, help="Event name as a string")
    parser.add_argument("task_metadata", type=str, help="JSON string")
    args = parser.parse_args()

    resource = Resource.create(
        {
            "service.name": os.getenv("AGENT_NAME"),
            "host.name": os.getenv("CIRCA_HOST"),
        }
    )
    logger_provider = LoggerProvider(resource=resource)
    logger_provider.add_log_record_processor(BatchLogRecordProcessor(ConsoleSpanExporter()))
    logger_provider.add_log_record_processor(BatchLogRecordProcessor(OTLPLogExporter(insecure=True)))
    handler = LoggingHandler(level=logging.NOTSET, logger_provider=logger_provider)

    logger = logging.getLogger()
    logger.addHandler(handler)
    logger.setLevel(logging.DEBUG)

    tracer_provider = TracerProvider(resource=resource)
    tracer_provider.add_span_processor(BatchSpanProcessor(ConsoleSpanExporter()))
    tracer_provider.add_span_processor(BatchSpanProcessor(OTLPSpanExporter(insecure=True)))
    set_tracer_provider(tracer_provider)

    with get_tracer("send-telemetry-event").start_as_current_span(args.action_category) as span:
        span.set_attribute("crs.action.category", args.action_category)
        span.set_attribute("crs.action.name", args.action_name)
        span.set_attribute("deployment.environment", f"{os.environ.get("CIRCA_BASENAME")}@{os.environ.get("CIRCA_HOST", os.uname().nodename)}")
        span.set_attribute("service.name", os.getenv("AGENT_NAME"))
        span.set_attribute("host.name", os.getenv("CIRCA_HOST"))

        try:
            task_metadata = json.loads(args.task_metadata)
            for key, value in task_metadata.items():
                span.set_attribute(key, value)
        except:
            logging.exception("Failed to deserialize task_metadata")

        span.set_status(Status(StatusCode.OK))
        span.add_event(args.event_name)
        logging.info("Sent event")
