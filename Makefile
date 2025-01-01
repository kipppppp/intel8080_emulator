
# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -g

# Build directory
BUILDDIR = build

# Files
SOURCES = cpu.c disassembler.c
OBJECTS = $(patsubst %.c, $(BUILDDIR)/%.o, $(SOURCES))
TARGET = $(BUILDDIR)/emulator

# Default target
all: $(TARGET)

# Linking the executable
$(TARGET): $(OBJECTS)
	mkdir -p $(BUILDDIR)
	$(CC) $(CFLAGS) -o $@ $^

# Compiling source files to object files
$(BUILDDIR)/%.o: %.c
	mkdir -p $(BUILDDIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Cleaning up build artifacts
clean:
	rm -rf $(BUILDDIR)

.PHONY: all clean
