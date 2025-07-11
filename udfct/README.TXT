---------------------------------------------------------------------------
 Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006

 All rights are reserved. Reproduction in whole or in part is
 prohibited without the written consent of the copyright owner.

 Author(s)   : Gerrit Scholl
---------------------------------------------------------------------------


At the moment compiled and tested platforms are:

* Microsoft Windows Vista and XP with Microsoft Visual Studio 2005
* Several Unix flavours like Linux, Sun Solaris, HP-UX, IRIX 64

The binaries are in the appropriate bin/* directories with
two versions for some platforms.
For configuration info see bin/*/make.log

Although these are the only platforms tested, this code has been
successfully compiled and used on other platforms, like Win2k,
Win9x, many Unix flavours and Mac.
We expect that the provided source code will successfully compile
and run on any platform for which an appropriate version of the
GNU c compiler is available.

For the main Makefile,   see: src/udf_tester/Makefile
Before make, first customize: src/udf_tester/Makefile.uct

- Apart from the Unix Makefiles, also Visual Studio 2005 solution and project
  files are included in the delivery packet (also suitable for the free Visual
  Studio Express Edition).

1) Open src/udfct.sln with Visual Studio 2005
   or with the free Visual Studio Express Edition.
2) set udf_tester as the startup project
3) "rebuild solution" will compile all

Limitations:

Media on SCSI/Atapi devices can only be accessed on Windows, Linux
and NetBSD. No NetBSD binaries are included in the package.
Further, on Irix 64 and more unix flavours, a drive is dealt with
in the same way as an image file, so without the -scsi option.
A drawback of this is that multisession and usage of a VAT cannot
be verified.

On all systems, an image file can be used as input.
The format of such an image file is a straight concatenation of
the UDF Volume Space sectors. An image file can also be a
concatenation of more than one chunk file.

Please read the FAQ.TXT file for more information on how to use
the verifier.


Disclaimer:

This code and executables are distributed 'as is'. Usage of this
code or any provided executables is entirely at your own risk.
Koninklijke Philips Electronics N.V. is not responsible for any
damage done either as a direct result of using this code or as
a side-effect of this.

Permission to distribute an altered version of the source code
or executables provided in this package is explicitely denied.

Detailed license information is in the file LICENSE.TXT

