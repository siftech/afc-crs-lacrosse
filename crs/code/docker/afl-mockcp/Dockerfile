FROM docker.io/library/aflplusplus:latest as afl

FROM mockcp:latest

# Note special ./ /. syntax should essentially cp -rf A/* B
COPY --from=afl ./AFLplusplus/. /AFLplusplus
COPY --from=afl ./usr/lib/. /usr/lib
COPY --from=afl ./bin/. /bin
COPY --from=afl ./usr/bin/. /usr/bin
COPY --from=afl ./usr/local/bin/. /usr/local/bin

WORKDIR /AFLplusplus
