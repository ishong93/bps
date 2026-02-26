CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -pedantic -g
LDFLAGS =

# Source files
CORE_SRCS = atms.c atre.c unify.c planner.c
TEST_SRCS = atest.c atret.c main.c

CORE_OBJS = $(CORE_SRCS:.c=.o)
TEST_OBJS = $(TEST_SRCS:.c=.o)
ALL_OBJS = $(CORE_OBJS) $(TEST_OBJS)

TARGET = atms_test

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(ALL_OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

%.o: %.c atms.h atre.h unify.h planner.h
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(ALL_OBJS) $(TARGET) $(TARGET).exe
