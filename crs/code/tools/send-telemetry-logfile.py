#!/usr/bin/env python3

import argparse
import logging
from opentelemetry.exporter.otlp.proto.grpc._log_exporter import OTLPLogExporter
from opentelemetry.sdk._logs import LoggerProvider, LoggingHandler
from opentelemetry.sdk._logs.export import BatchLogRecordProcessor
from opentelemetry.sdk.resources import Resource
import os
from pathlib import Path
from subprocess import Popen, run, PIPE
from typing import Generator


def tail_follow(path: Path) -> Generator[str, None, None]:
    """
    Yields lines returned by tail -f.
    """

    with Popen(["tail", "-f", path], stdout=PIPE) as proc:
        assert proc.stdout != None
        while True:
            line = proc.stdout.readline()
            if not line:
                return
            yield line.decode("utf-8", "backslashreplace")


if __name__ == "__main__":
    missing_vars = [
        var
        for var in [
            "OTEL_EXPORTER_OTLP_ENDPOINT",
            "OTEL_EXPORTER_OTLP_HEADERS",
            "OTEL_EXPORTER_OTLP_PROTOCOL",
        ]
        if var not in os.environ
    ]
    if len(missing_vars) != 0:
        print(f"Missing environment variable(s): {', '.join(missing_vars)}")
        exit(100)

    parser = argparse.ArgumentParser(description="send-telemetry-logfile")
    parser.add_argument("agent", type=str, help="agent name")
    parser.add_argument("logfile", type=str, help="path to the log file")
    args = parser.parse_args()

    logger_provider = LoggerProvider(
        resource=Resource.create(
            {
                "service.name": args.agent,
                "deployment.environment": f"{os.environ.get("CIRCA_BASENAME")}@{os.environ.get("CIRCA_HOST", os.uname().nodename)}",
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

    log_file = Path(args.logfile)
    run([Path(__file__).parent / "wait-for-file", log_file])
    for line in tail_follow(Path(log_file)):
        logging.debug(line)

    logger_provider.shutdown()
