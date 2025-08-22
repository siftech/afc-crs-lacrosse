#!/usr/bin/env python
import argparse
import openlit
import os

import logging

from enum import Enum
from opentelemetry.exporter.otlp.proto.grpc._log_exporter import OTLPLogExporter
from opentelemetry.sdk._logs import LoggerProvider, LoggingHandler
from opentelemetry.sdk._logs.export import BatchLogRecordProcessor
from opentelemetry.sdk.resources import Resource

# FIXME quick hack to map ints to log levels and restrict input args to known levels
class LogLevel(str, Enum):
    DEBUG = logging.DEBUG
    INFO = logging.INFO
    WARNING = logging.WARNING
    ERROR = logging.ERROR
    FATAL = logging.FATAL

    def __str__(self):
        return str((self._name_, self._value_))

# Initialize openlit
openlit.init(
    application_name="lacrosse",
)

def check_environment():
    error = False
    if "OTEL_EXPORTER_OTLP_HEADERS" not in os.environ:
        print("OTEL_EXPORTER_OTLP_HEADERS must be set")
        error = True

    if "OTEL_EXPORTER_OTLP_ENDPOINT" not in os.environ:
        print("OTEL_EXPORTER_OTLP_ENDPOINT must be set")
        error = True

    if "OTEL_EXPORTER_OTLP_PROTOCOL" not in os.environ:
        print("OTEL_EXPORTER_OTLP_PROTOCOL must be set")
        error = True

    if error:
        exit(1)

if __name__ == '__main__':
    check_environment()
    parser = argparse.ArgumentParser(description = 'send-telemetry-log')
    parser.add_argument('agent', type=str, help='agent name')
    parser.add_argument('level', type=LogLevel, choices=list(LogLevel), help='log level')
    parser.add_argument('message', type=str, help='the message to log')
    args = parser.parse_args()

    logger_provider = LoggerProvider(
        resource=Resource.create(
            {
                "deployment.environment": f"{os.environ.get("CIRCA_BASENAME")}@{os.environ.get("CIRCA_HOST", os.uname().nodename)}",
                "service.name": args.agent,
                "host.name": os.environ.get("CIRCA_HOST", os.uname().nodename),
            }
        ),
    )
    otlp_exporter = OTLPLogExporter(insecure=True)
    logger_provider.add_log_record_processor(BatchLogRecordProcessor(otlp_exporter))
    handler = LoggingHandler(level=logging.NOTSET, logger_provider=logger_provider)

    logger = logging.getLogger()
    logger.addHandler(handler)
    logger.setLevel(logging.DEBUG)

    logging.log(int(args.level), args.message)
    logger_provider.shutdown()
