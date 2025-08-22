Command line used to find this crash:

/neo-fuzz/code/AFLplusplus/afl-fuzz -n -m none -i /neo-fuzz/code/etc/default-afl-inputs -o /neo-fuzz/code/lacrosse-targets/dvrf-pwnable/heap-overflow-stdin/out/ -s 42 -T FUZZBOMB1-AFL /neo-fuzz/code/lacrosse-targets/dvrf-pwnable/heap-overflow-stdin/out/heap_overflow_01_stdin.exe

If you can't reproduce a bug outside of afl-fuzz, be sure to set the same
memory limit. The limit used for this fuzzing session was 0 B.

Need a tool to minimize test cases before investigating the crashes or sending
them to a vendor? Check out the afl-tmin that comes with the fuzzer!

Found any cool bugs in open-source tools using afl-fuzz? If yes, please drop
an mail at <afl-users@googlegroups.com> once the issues are fixed

  https://github.com/AFLplusplus/AFLplusplus

