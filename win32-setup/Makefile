CFLAGS = -O3 -s -DWINVER=0x0500 -D_WIN32_IE=0x0500 -D_WIN32_WINNT=0x0500

all:
	gcc $(CFLAGS) -c ErgoEmacs.c -o ErgoEmacs.o 
	gcc $(CFLAGS) -c string_utils.c -o string_utils.o
	windres -O coff -o ErgoEmacs.res -i ErgoEmacs.rc
	gcc -s -o ErgoEmacs.exe ErgoEmacs.o string_utils.o ErgoEmacs.res -mwindows
	rm *.o ErgoEmacs.res

test:
	gcc -DTEST -mconsole -o string_utils.exe string_utils.c
