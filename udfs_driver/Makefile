# Makefile for UDFS Driver
# Based on UDFCT source code

# Common definitions from UDFCT
include ../udfct/udf_tester/Makefile.uct

# Paths
UDFS_SRC_DIR = src
UDFS_INC_DIR = include
UDFCT_CORE_DIR = ../udfct/uct_core
UDFCT_SCSI_DIR = ../udfct/udf_scsi

# Compiler settings
INCS = -I$(UDFS_INC_DIR) -I$(UDFCT_CORE_DIR) -I$(UDFCT_SCSI_DIR)
CFLAGS = -O3 $(DEBUG) $(INCS) $(PLATFORM) $(PLATFORM_CFLAGS) $(PLATFORM_SZ64FLAGS)

# Libraries
UDFS_OBJS = $(UDFS_SRC_DIR)/udfs.o
UDFS_LIB = libudfs.a

# Linking
DFLAGS = -L$(UDFCT_CORE_DIR) -L$(UDFCT_SCSI_DIR) -L.
LIBS = -ludfct -ludfsc $(PLATFORM_LIBS)

# Targets
.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

all: lib

lib: deps $(UDFS_LIB)

deps:
	@echo "Building UDFCT dependencies..."
	( cd $(UDFCT_CORE_DIR) ; $(MAKE) lib )
	( cd $(UDFCT_SCSI_DIR) ; $(MAKE) lib )

$(UDFS_LIB): $(UDFS_OBJS)
	rm -f $(UDFS_LIB)
	ar -rc $(UDFS_LIB) $(UDFS_OBJS)
	$(PLATFORM_RANLIB) $(UDFS_LIB)

example: lib
	$(CC) $(CFLAGS) $(DFLAGS) -o udfs_example examples/simple_example.c -ludfs $(LIBS)

test: lib
	$(CC) $(CFLAGS) $(DFLAGS) -o udfs_test test/test_udfs.c -ludfs $(LIBS)

clean:
	rm -f $(UDFS_OBJS) $(UDFS_LIB) udfs_example udfs_test

clean_all: clean
	( cd $(UDFCT_CORE_DIR) ; $(MAKE) clean )
	( cd $(UDFCT_SCSI_DIR) ; $(MAKE) clean )

# Header dependencies
$(UDFS_SRC_DIR)/udfs.o: $(UDFS_INC_DIR)/udfs.h $(UDFCT_CORE_DIR)/uct_core.h

.PHONY: all lib deps example test clean clean_all