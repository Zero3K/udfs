# Main Makefile for UDF verifier
# See customizing explanation in Makefile.uct
#

include Makefile.uct	# common customizing

CFILES  = udf_test.c commandline.c write_image.c selectdevice.c img_device.c \
	 scsi_device.c mfile.c platform.c raw_device.c drive_device.c
OBJS    = udf_test.o commandline.o write_image.o selectdevice.o img_device.o \
	 scsi_device.o mfile.o platform.o raw_device.o drive_device.o

## DEBUG = -g

INCS	= -I../uct_core -I../udf_scsi
DFLAGS  = -L../uct_core -L../udf_scsi
LIBS    = -ludfct -ludfsc $(PLATFORM_LIBS)
CFLAGS  = -O3 $(DEBUG) $(INCS) $(PLATFORM) $(PLATFORM_CFLAGS)	\
	      $(PLATFORM_SZ64FLAGS) $(SCSI_SUPPORT) $(RAW_SUPPORT)

BINOUT  = udf_test

.c.o:
	$(CC) $(CFLAGS) -c $<

all:
	$(MAKE) -i identification	# ignore identification errors
	$(CC) -v
	@echo ""
	@echo "Be sure that customizing definitions in ../udf_tester/Makefile.uct are correct"
	@echo "=============================================================================="
	@echo ""
	( cd ../uct_core ; $(MAKE) all )
	( cd ../udf_scsi ; $(MAKE) all )
	$(MAKE) verifier

verifier: $(OBJS)
	$(CC) $(CFLAGS) $(DFLAGS) -o $(BINOUT) $(OBJS) $(LIBS)

clean:
	(rm -f $(OBJS) $(BINOUT))

clean_all:
	( cd ../uct_core ; $(MAKE) clean )
	( cd ../udf_scsi ; $(MAKE) clean )
	$(MAKE) clean

identification:
	@echo "Compiling system identification:"
	@uname -a

## header file dependencies:

commandline.o: commandline.h mfile.h platform.h write_image.h ../uct_core/crc.h	\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uct_core.h ../uct_core/uctdata.h ../uct_core/uctendian.h	\
	../uct_core/ucterror.h ../uct_core/uctfiles.h ../uct_core/uctgeneral.h	\
	../uct_core/uctmedium.h ../uct_core/uctnodes.h ../uct_core/uctstatus.h	\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

drive_device.o: commandline.h drive_device.h platform.h ../uct_core/crc.h	\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uct_core.h ../uct_core/uctdata.h ../uct_core/uctendian.h	\
	../uct_core/ucterror.h ../uct_core/uctfiles.h ../uct_core/uctgeneral.h	\
	../uct_core/uctmedium.h ../uct_core/uctnodes.h ../uct_core/uctstatus.h	\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

img_device.o: commandline.h img_device.h mfile.h platform.h ../uct_core/crc.h	\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uct_core.h ../uct_core/uctdata.h ../uct_core/uctendian.h	\
	../uct_core/ucterror.h ../uct_core/uctfiles.h ../uct_core/uctgeneral.h	\
	../uct_core/uctmedium.h ../uct_core/uctnodes.h ../uct_core/uctstatus.h	\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

mfile.o: mfile.h platform.h ../uct_core/crc.h ../uct_core/device.h		\
	../uct_core/general.h ../uct_core/mytypes.h ../uct_core/uct_core.h	\
	../uct_core/uctdata.h ../uct_core/uctendian.h ../uct_core/ucterror.h	\
	../uct_core/uctfiles.h ../uct_core/uctgeneral.h ../uct_core/uctmedium.h	\
	../uct_core/uctnodes.h ../uct_core/uctstatus.h				\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

platform.o: platform.h ../uct_core/crc.h ../uct_core/device.h			\
	../uct_core/general.h ../uct_core/mytypes.h ../uct_core/uct_core.h	\
	../uct_core/uctdata.h ../uct_core/uctendian.h ../uct_core/ucterror.h	\
	../uct_core/uctfiles.h ../uct_core/uctgeneral.h ../uct_core/uctmedium.h	\
	../uct_core/uctnodes.h ../uct_core/uctstatus.h				\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

raw_device.o: commandline.h platform.h raw_device.h ../uct_core/crc.h		\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uct_core.h ../uct_core/uctdata.h ../uct_core/uctendian.h	\
	../uct_core/ucterror.h ../uct_core/uctfiles.h ../uct_core/uctgeneral.h	\
	../uct_core/uctmedium.h ../uct_core/uctnodes.h ../uct_core/uctstatus.h	\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

scsi_device.o: commandline.h platform.h scsi_device.h version.h 		\
	../udf_scsi/scsiapi.h ../udf_scsi/scsidefs.h ../uct_core/crc.h 		\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uct_core.h ../uct_core/uctdata.h ../uct_core/uctendian.h	\
	../uct_core/ucterror.h ../uct_core/uctfiles.h ../uct_core/uctgeneral.h	\
	../uct_core/uctmedium.h ../uct_core/uctnodes.h ../uct_core/uctstatus.h	\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

selectdevice.o: commandline.h drive_device.h img_device.h platform.h		\
	raw_device.h scsi_device.h selectdevice.h ../udf_scsi/scsiapi.h		\
	../udf_scsi/scsidefs.h ../uct_core/crc.h ../uct_core/device.h		\
	../uct_core/general.h ../uct_core/mytypes.h ../uct_core/uct_core.h	\
	../uct_core/uctdata.h ../uct_core/uctendian.h ../uct_core/ucterror.h	\
	../uct_core/uctfiles.h ../uct_core/uctgeneral.h ../uct_core/uctmedium.h	\
	../uct_core/uctnodes.h ../uct_core/uctstatus.h				\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

udf_test.o: commandline.h drive_device.h mfile.h platform.h raw_device.h	\
	scsi_device.h selectdevice.h version.h write_image.h			\
	../udf_scsi/scsiapi.h ../udf_scsi/scsidefs.h ../uct_core/crc.h		\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uct_core.h ../uct_core/uctdata.h ../uct_core/uctendian.h	\
	../uct_core/ucterror.h ../uct_core/uctfarapart.h ../uct_core/uctfiles.h	\
	../uct_core/uctgeneral.h ../uct_core/uctmedium.h ../uct_core/uctnodes.h	\
	../uct_core/uctstatus.h ../uct_core/ucttimestamp.h			\
	../uct_core/uctverify.h ../uct_core/uctversion.h			\
	../uct_core/udfstruct.h ../uct_core/unicode.h

write_image.o: mfile.h platform.h scsi_device.h write_image.h			\
	../udf_scsi/scsiapi.h ../udf_scsi/scsidefs.h ../uct_core/crc.h		\
	../uct_core/device.h ../uct_core/general.h ../uct_core/mytypes.h	\
	../uct_core/uct_core.h ../uct_core/uctdata.h ../uct_core/uctendian.h	\
	../uct_core/ucterror.h ../uct_core/uctfiles.h ../uct_core/uctgeneral.h	\
	../uct_core/uctmedium.h ../uct_core/uctnodes.h ../uct_core/uctstatus.h	\
	../uct_core/ucttimestamp.h ../uct_core/uctverify.h			\
	../uct_core/uctversion.h ../uct_core/udfstruct.h ../uct_core/unicode.h

