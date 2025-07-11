# Makefile for uct_core library
#

include ../udf_tester/Makefile.uct	# common customizing

## DEBUG = -g

INCS	=
CFLAGS  = -O3 $(DEBUG) $(INCS) $(PLATFORM_CFLAGS) $(PLATFORM_SZ64FLAGS)

OBJS =	crc.o device.o general.o ucterror.o uctgeneral.o uctverify.o	\
	unicode.o uctstatus.o uctendian.o uctmedium.o uctfarapart.o	\
	uctfiles.o uctallocation.o ucttimestamp.o uctnodes.o
SRC  =	crc.c device.c general.c ucterror.c uctgeneral.c uctverify.c	\
	unicode.c uctstatus.c uctendian.c uctmedium.c uctfarapart.c	\
	uctfiles.c uctallocation.c ucttimestamp.c uctnodes.c

LIBOUT  = libudfct.a

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

crc.o: crc.h general.h mytypes.h

device.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h uctgeneral.h	\
	uctmedium.h uctnodes.h udfstruct.h unicode.h

general.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h uctgeneral.h	\
	uctmedium.h uctnodes.h udfstruct.h unicode.h

uctallocation.o: crc.h device.h general.h mytypes.h uctallocation.h uctdata.h	\
	ucterror.h uctgeneral.h uctmedium.h uctnodes.h uctstatus.h udfstruct.h	\
	unicode.h

uctendian.o: crc.h device.h general.h mytypes.h uctdata.h uctendian.h		\
	ucterror.h uctgeneral.h uctmedium.h uctnodes.h uctstatus.h udfstruct.h	\
	unicode.h

ucterror.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h uctfiles.h	\
	uctgeneral.h uctmedium.h uctnodes.h udfstruct.h unicode.h

uctfarapart.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h		\
	uctfarapart.h uctfiles.h uctgeneral.h uctmedium.h uctnodes.h		\
	uctstatus.h udfstruct.h unicode.h

uctfiles.o: crc.h device.h general.h mytypes.h uctallocation.h uctdata.h	\
	uctendian.h ucterror.h uctfarapart.h uctfiles.h uctgeneral.h		\
	uctmedium.h uctnodes.h uctstatus.h ucttimestamp.h uctverify.h		\
	udfstruct.h unicode.h

uctgeneral.o: crc.h device.h general.h mytypes.h uctallocation.h uctdata.h	\
	uctendian.h ucterror.h uctfiles.h uctgeneral.h uctmedium.h uctnodes.h	\
	uctstatus.h uctverify.h udfstruct.h unicode.h

uctmedium.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h		\
	uctgeneral.h uctmedium.h uctnodes.h uctstatus.h udfstruct.h unicode.h

uctnodes.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h uctfiles.h	\
	uctgeneral.h uctmedium.h uctnodes.h uctstatus.h udfstruct.h unicode.h

uctstatus.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h		\
	uctgeneral.h uctmedium.h uctnodes.h uctstatus.h ucttimestamp.h		\
	uctversion.h udfstruct.h unicode.h

ucttimestamp.o: crc.h device.h general.h mytypes.h uctdata.h ucterror.h		\
	uctgeneral.h uctmedium.h uctnodes.h uctstatus.h ucttimestamp.h		\
	udfstruct.h unicode.h

uctverify.o: crc.h device.h general.h mytypes.h uctallocation.h uctdata.h	\
	uctendian.h ucterror.h uctfiles.h uctgeneral.h uctmedium.h uctnodes.h	\
	uctstatus.h ucttimestamp.h uctverify.h udfstruct.h unicode.h

unicode.o: general.h mytypes.h unicode.h

