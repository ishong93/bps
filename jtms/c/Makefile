# Makefile for JTMS/JTRE C implementation
# Converted from Common Lisp "Building Problem Solvers" codebase

CC = gcc
CFLAGS = -Wall -Wextra -g -std=c11
LDFLAGS = -lm

# Source files
CORE_SRCS = jtms.c jtre.c jdata.c jrules.c funify.c
MATCH_SRCS = match.c simplify.c
QUEENS_SRCS = jqueens.c jqrule.c
JSAINT_SRCS = jsaint.c jsops.c jsrules.c
TEST_SRCS = jtest.c jtms_ex.c jtms_loader.c

ALL_SRCS = $(CORE_SRCS) $(MATCH_SRCS) $(QUEENS_SRCS) $(JSAINT_SRCS) $(TEST_SRCS)
ALL_OBJS = $(ALL_SRCS:.c=.o)

# Header files
HEADERS = jtms.h jtre.h jdata.h jrules.h funify.h \
          match.h simplify.h \
          jqueens.h jqrule.h \
          jsaint.h jsops.h jsrules.h \
          jtest.h jtms_ex.h jtms_loader.h

# Default target
all: jtms_test jtms_examples

# JTMS test program (jtest + core)
jtms_test: jtest.o $(CORE_SRCS:.c=.o) $(MATCH_SRCS:.c=.o) jtms_loader.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# JTMS examples program
jtms_examples: jtms_ex.o $(CORE_SRCS:.c=.o)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# N-Queens program
jqueens: $(QUEENS_SRCS:.c=.o) $(CORE_SRCS:.c=.o) $(MATCH_SRCS:.c=.o)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# JSAINT program
jsaint: $(JSAINT_SRCS:.c=.o) $(CORE_SRCS:.c=.o) $(MATCH_SRCS:.c=.o) jtms_loader.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Static library
libjtms.a: $(ALL_OBJS)
	ar rcs $@ $^

# Object file rules
%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Clean
clean:
	rm -f *.o jtms_test jtms_examples jqueens jsaint libjtms.a

# Phony targets
.PHONY: all clean
