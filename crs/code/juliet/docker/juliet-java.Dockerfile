FROM ubuntu:22.04 as build

ARG JULIET_PATH=/workspace/juliet

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
      ant\
      dos2unix \
      openjdk-11-jdk-headless \
      python3 \
      python3-pip \
    && rm -rf /var/lib/apt/lists/*

ENV JULIET_PATH ${JULIET_PATH}
ADD ./java/juliet/Java ${JULIET_PATH}

# patch scripts
ARG SUBSHEBANG="1 s|^.*\$|\#\!\/usr\/bin\/env python3|"
RUN sed -i "${SUBSHEBANG}" "${JULIET_PATH}/create_per_cwe_files.py" \
    && sed -i "${SUBSHEBANG}" "${JULIET_PATH}/py_common.py" \
    && sed -i "${SUBSHEBANG}" "${JULIET_PATH}/run_analysis_example_tool.py" \
    && sed -i "${SUBSHEBANG}" "${JULIET_PATH}/update_Main_java_ServletMain_java_and_web_xml.py" \
    && sed -i 's|src\\\\testcases|src\/testcases|' "${JULIET_PATH}/run_analysis_example_tool.py" \
    && sed -i 's|\.\.\\\\|\.\.\/|g' "${JULIET_PATH}/py_common.py"

WORKDIR ${JULIET_PATH}

# dos2unix on juliet files
RUN find . -type f -print0 | xargs -0 dos2unix \
    && ant clean

# Add util scripts
ADD ./java/scripts /workspace/scripts
RUN chmod +x \
      ${JULIET_PATH}/create_per_cwe_files.py \
      ${JULIET_PATH}/run_analysis_example_tool.py \
      /workspace/scripts/*.sh


# Initial cleanup
RUN /workspace/scripts/clean-juliet-testcases.sh \
    && /workspace/scripts/generate-for-top-25.sh

# Final image
FROM juliet-analysis-tool:latest

ARG JULIET_PATH=/workspace/juliet

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
      ant\
      openjdk-11-jdk-headless \
      python3 \
      python3-pip \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build ${JULIET_PATH} ${JULIET_PATH}
COPY --from=build /workspace/scripts /workspace/scripts
ENV PATH /workspace/scripts:${PATH}
ENV JULIET_PATH ${JULIET_PATH}
WORKDIR ${JULIET_PATH}

