// Automatically generated file.
#include"gparams.h"
#include"symbol.h"
#include"trace.h"
#include"debug.h"
#include"rational.h"
#include"prime_generator.h"
void mem_initialize() {
initialize_symbols();
rational::initialize();
gparams::init();
}
void mem_finalize() {
gparams::finalize();
finalize_symbols();
finalize_trace();
finalize_debug();
rational::finalize();
prime_iterator::finalize();
}
