CC=/usr/bin/gcc
CPP=/usr/bin/g++
DEBUG=-g
CFLAGS=-fsanitize=address $(INCLUDES)
LFLAGS=-lpthread -lm
LD=ld
INCLUDE_MAIN=-DINCLUDEMAIN -DOMITGOOD

# Debugging information
$(info OUT_C_BAD_FILES: $(OUT_C_BAD_FILES))
$(info C_BAD_FILES: $(C_BAD_FILES))
$(info OUT_CPP_BAD_FILES: $(OUT_CPP_BAD_FILES))

MAIN=main_linux.cpp
MAIN_OBJECT=$(MAIN:.cpp=.o)

#../../testcasesupport/
#Assumes user has defined the LACROSSE_HOME environment variable
TOOLS_PATH=$(LACROSSE_HOME)/code/tools/
C_SUPPORT_PATH=$(TOOLS_PATH)testcasesupport/
C_SUPPORT_FILES=$(C_SUPPORT_PATH)io.c $(C_SUPPORT_PATH)std_thread.c
C_SUPPORT_OBJECTS=io.o std_thread.o
# socket and fscanf examples dont actually have ways to send in faulting input
FILTER_OUT=$(wildcard CWE*w32*.c*) $(wildcard CWE*wchar_t*.c*) $(wildcard *socket*.c*) $(wildcard *fscanf*.c*)

#INCLUDES=-I ../../testcasesupport
INCLUDES=-I $(C_SUPPORT_PATH)

# only grab the .c files without "w32" or "wchar_t" in the name
C_SOURCES=$(filter-out $(FILTER_OUT),$(wildcard CWE*.c))
C_OBJECTS=$(C_SOURCES:.c=.o)

# only grab the .cpp files without "w32" or "wchar_t" in the name
CPP_SOURCES=$(filter-out $(FILTER_OUT),$(wildcard CWE*.cpp))
CPP_OBJECTS=$(CPP_SOURCES:.cpp=.o)

#NOTE: $(wildcard CWE*_#*.c*) might include all the lettered versions of each #, but this somehow duplicates the target (undesired)---use what is given for SIMPLES
SIMPLES=$(filter-out $(FILTER_OUT), $(wildcard CWE*0.c*) $(wildcard CWE*1.c*) $(wildcard CWE*2.c*) $(wildcard CWE*3.c*) $(wildcard CWE*4.c*)) \
        $(filter-out $(FILTER_OUT), $(wildcard CWE*5.c*) $(wildcard CWE*6.c*) $(wildcard CWE*7.c*) $(wildcard CWE*8.c*) $(wildcard CWE*9.c*))
SIMPLES_C=$(filter-out $(CPP_SOURCES), $(SIMPLES))
SIMPLES_CPP=$(filter-out $(C_SOURCES), $(SIMPLES))

#Letter ending in some filename cases rather than a number ending
LETTEREDSA_C=$(filter-out $(FILTER_OUT), $(wildcard CWE*a.c))
LETTEREDSA_CPP=$(filter-out $(FILTER_OUT), $(wildcard CWE*a.cpp))

LETTEREDSB_C=$(filter-out $(FILTER_OUT), $(wildcard CWE*b.c))
LETTEREDSB_CPP=$(filter-out $(FILTER_OUT), $(wildcard CWE*b.cpp))

LETTEREDSC_C=$(filter-out $(FILTER_OUT), $(wildcard CWE*c.c))
LETTEREDSC_CPP=$(filter-out $(FILTER_OUT), $(wildcard CWE*c.cpp))

LETTEREDSD_C=$(filter-out $(FILTER_OUT), $(wildcard CWE*d.c))
LETTEREDSD_CPP=$(filter-out $(FILTER_OUT), $(wildcard CWE*d.cpp))

LETTEREDSE_C=$(filter-out $(FILTER_OUT), $(wildcard CWE*e.c))
LETTEREDSE_CPP=$(filter-out $(FILTER_OUT), $(wildcard CWE*e.cpp))

LETTEREDSF_C=$(filter-out $(FILTER_OUT), $(wildcard CWE*f.c))
LETTEREDSF_CPP=$(filter-out $(FILTER_OUT), $(wildcard CWE*f.cpp))

LETTEREDS_C=$(LETTEREDSA_C) $(LETTEREDSB_C) $(LETTEREDSC_C) $(LETTEREDSD_C) $(LETTEREDSE_C) $(LETTEREDSF_C)
LETTEREDS_CPP=$(LETTEREDSA_CPP) $(LETTEREDSB_CPP) $(LETTEREDSC_CPP) $(LETTEREDSD_CPP) $(LETTEREDSE_CPP) $(LETTEREDSF_CPP)

GOOD1S=$(filter-out $(FILTER_OUT), $(wildcard CWE*_good1.cpp))
BADS=$(subst _good1.,_bad.,$(GOOD1S))

INDIVIDUALS_C=$(addsuffix .out, $(sort $(subst .c,,$(SIMPLES_C) )))
INDIVIDUALS_CPP=$(addsuffix .out, $(sort $(subst .cpp,,$(SIMPLES_CPP) )))

#INDIVIDUALS_C=$(addsuffix .out, $(sort $(subst .c,,$(SIMPLES_C) $(LETTEREDS_C))))
#INDIVIDUALS_CPP=$(addsuffix .out, $(sort $(subst .cpp,,$(SIMPLES_CPP) $(LETTEREDS_CPP) $(BADS) $(GOOD1S))))

OBJECTS=$(MAIN_OBJECT) $(C_OBJECTS) $(CPP_OBJECTS) $(C_SUPPORT_OBJECTS)
# TARGET is the only line in this file specific to the CWE
TARGET=CWE366

#The code versions of the good-function-only and bad-function-only C and CPP files
#C_BAD_FILES=$(addsuffix .bad.c, $(sort $(subst .c,,$(SIMPLES_C) $(LETTEREDS_C))))
C_BAD_FILES=$(addsuffix .bad.c, $(sort $(subst .c,,$(SIMPLES_C))))
C_GOOD_FILES=$(addsuffix .good.c, $(sort $(subst .c,,$(SIMPLES_C) $(LETTEREDS_C))))
#CPP_BAD_FILES=$(addsuffix .bad.cpp, $(sort $(subst .cpp,,$(SIMPLES_CPP) $(LETTEREDS_CPP) $(BADS) $(GOOD1S))))
CPP_BAD_FILES=$(addsuffix .bad.cpp, $(sort $(subst .cpp,,$(SIMPLES_CPP))))
CPP_GOOD_FILES=$(addsuffix .good.cpp, $(sort $(subst .cpp,,$(SIMPLES_CPP) $(LETTEREDS_CPP) $(BADS) $(GOOD1S))))

#The output versions of the good-function-only and bad-function-only C and CPP files
OUT_C_BAD_FILES=$(addsuffix .bad.out, $(sort $(subst .c,,$(SIMPLES_C) )))
#OUT_C_BAD_FILES=$(addsuffix .bad.out, $(sort $(subst .c,,$(SIMPLES_C) $(LETTEREDS_C))))
OUT_C_GOOD_FILES=$(addsuffix .good.out, $(sort $(subst .c,,$(SIMPLES_C) $(LETTEREDS_C))))
#OUT_CPP_BAD_FILES=$(addsuffix .bad.out, $(sort $(subst .cpp,,$(SIMPLES_CPP) $(LETTEREDS_CPP) $(BADS) $(GOOD1S))))
OUT_CPP_BAD_FILES=$(addsuffix .bad.out, $(sort $(subst .cpp,,$(SIMPLES_CPP) )))
OUT_CPP_GOOD_FILES=$(addsuffix .good.out, $(sort $(subst .cpp,,$(SIMPLES_CPP) $(LETTEREDS_CPP) $(BADS) $(GOOD1S))))

#all: $(TARGET)
all: individuals

partial.o: $(C_OBJECTS) $(CPP_OBJECTS)
	$(LD) -r $(C_OBJECTS) $(CPP_OBJECTS) -o $@

$(INDIVIDUALS_C): $(OUT_C_BAD_FILES) #$(OUT_C_GOOD_FILES) #$(C_SUPPORT_OBJECTS)

#$(INDIVIDUALS_C): $(OUT_C_BAD_FILES) #$(OUT_C_GOOD_FILES) #$(C_SUPPORT_OBJECTS)
#	#$(CC) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .out,,$@)*.bad.c) $(C_SUPPORT_OBJECTS) $(LFLAGS)
#	#$(CC) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .out,,$@)*.good.c) $(C_SUPPORT_OBJECTS) $(LFLAGS)
#	#$(CC) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .out,,$@)*.c) $(C_SUPPORT_OBJECTS) $(LFLAGS)
#
#$(INDIVIDUALS_CPP): $(OUT_CPP_BAD_FILES) # $(OUT_CPP_GOOD_FILES) #$(C_SUPPORT_OBJECTS)
#	#$(CPP) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .bad.out,,$@)*.bad.cpp) $(C_SUPPORT_OBJECTS) $(LFLAGS)
#	#$(CPP) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .good.out,,$@)*.good.cpp) $(C_SUPPORT_OBJECTS) $(LFLAGS)
#	#$(CPP) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .out,,$@)*.cpp) $(C_SUPPORT_OBJECTS) $(LFLAGS)
#
#individuals: $(INDIVIDUALS_C) $(INDIVIDUALS_CPP) #$(INDIVIDUALS_BADC) $(INDIVIDUALS_BADCPP)
individuals: $(OUT_C_BAD_FILES) $(OUT_CPP_BAD_FILES) #$(INDIVIDUALS_BADC) $(INDIVIDUALS_BADCPP)
	echo "Processing0"
#	$(TOOLS_PATH)set-aside-isnt-always-bad `pwd`
#	$(TOOLS_PATH)collect-bad-outputs `pwd`
#	$(TOOLS_PATH)organize-files `pwd`

$(OUT_C_BAD_FILES): $(C_BAD_FILES)
	#$(CC) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .out,,$@)*.c) $(C_SUPPORT_OBJECTS) $(LFLAGS)
	echo "Processing1"
	$(CC) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(subst .out,.c,$@) $(C_SUPPORT_OBJECTS) $(LFLAGS)

$(OUT_C_GOOD_FILES): $(C_GOOD_FILES)
	#$(CC) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .out,,$@)*.c) $(C_SUPPORT_OBJECTS) $(LFLAGS)
	$(CC) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(subst .out,.c,$@) $(C_SUPPORT_OBJECTS) $(LFLAGS)

$(OUT_CPP_BAD_FILES): $(CPP_BAD_FILES)
	#$(CPP) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(wildcard $(subst .out,,$@)*.cpp) $(C_SUPPORT_OBJECTS) $(LFLAGS)
	$(CPP) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(subst .out,.cpp,$@) $(C_SUPPORT_OBJECTS) $(LFLAGS)

$(OUT_CPP_GOOD_FILES): $(CPP_GOOD_FILES)
	$(CPP) -fsanitize=address $(DEBUG) $(INCLUDES) $(INCLUDE_MAIN) -o $@ $(subst .out,.cpp,$@) $(C_SUPPORT_OBJECTS) $(LFLAGS)

$(C_BAD_FILES): $(C_SUPPORT_OBJECTS)
	$(TOOLS_PATH)justbad $(subst .bad.c,.c,$@) | cpp -dD -P -fpreprocessed |sed 's/bad/stuff/gi' |sed 's/CWE.*__/foofoo/g' |sed 's/CWE/boo/g'> $@
	#$(TOOLS_PATH)justbad $(wildcard $(subst .bad.c,,$@).c) > $(subst .c,.c,$@)

$(C_GOOD_FILES): $(C_SUPPORT_OBJECTS)
	$(TOOLS_PATH)justgood $(subst .good.c,.c,$@) > $@

$(CPP_BAD_FILES): $(C_SUPPORT_OBJECTS)
	#$(TOOLS_PATH)justbad $(subst .bad.cpp,.cpp,$@) | cpp -dD -P -fpreprocessed |sed 's/bad/stuff/gi' > $@
	$(TOOLS_PATH)justbad $(subst .bad.cpp,.cpp,$@) | cpp -dD -P -fpreprocessed |sed 's/bad/stuff/gi' |sed 's/CWE.*__/foofoo/g' |sed 's/CWE/boo/g'> $@

$(CPP_GOOD_FILES): $(C_SUPPORT_OBJECTS)
	$(TOOLS_PATH)justgood $(subst .good.cpp,.cpp,$@) > $@

$(TARGET) : $(OBJECTS)
	$(CPP) $(LFLAGS) $(OBJECTS) -o $(TARGET)

%.out:%.c 
	$(CC) $(CFLAGS) $(INCLUDES) $(INCLUDE_MAIN) $(C_SUPPORT_FILES) $^ -o $@

$(CPP_OBJECTS) : %.o:%.cpp
	$(CPP) $(CFLAGS) $(INCLUDES) $^ -o $@

$(C_SUPPORT_OBJECTS) : $(C_SUPPORT_FILES)
	$(CC) $(CFLAGS) $(INCLUDES) $(C_SUPPORT_PATH)$(@:.o=.c) -o $@

$(MAIN_OBJECT) : $(MAIN)
	$(CC) $(CFLAGS) $(INCLUDES) $(MAIN) -o $@

clean:
	rm -rf *.o *.out *.bad.c *.good.c $(TARGET) juliet_code_cleaned juliet_exec_cleaned sanitized_files stdout_files
