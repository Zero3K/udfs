# Makefile for udf_scsi library
#

include ../udf_tester/Makefile.uct	# common customizing

## DEBUG = -g

INCS	= -I../uct_core
CFLAGS  = -O3 $(DEBUG) $(INCS) $(PLATFORM) $(PLATFORM_CFLAGS)	\
	      $(PLATFORM_SZ64FLAGS) $(SCSI_SUPPORT)

OBJS =	scsiapi.o

SRC  =	scsiapi.c

LIBOUT  = libudfsc.a

.c.o:
	$(CC) $(CFLAGS) -c $<

all:	lib

lib:	$(OBJS)
	rm -f $(LIBOUT)
	ar -rc $(LIBOUT) $(OBJS)
	$(PLATFORM_RANLIB) $(LIBOUT)

clean:
	(rm -f $(OBJS) $(LIBOUT))

## header file dependencies:

scsiapi.o: scsiapi.h scsidefs.h	\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uctdata.h ../uct_core/ucterror.h ../uct_core/uctmedium.h	\
	../uct_core/uctnodes.h ../uct_core/udfstruct.h ../uct_core/unicode.h

