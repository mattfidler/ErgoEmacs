all:
	gcc -O3 -s -c ErgoEmacs.c -o ErgoEmacs.o -DWINVER=0x0500 -D_WIN32_IE=0x0500 -D_WIN32_WINNT=0x0500
	windres -O coff -o ErgoEmacs.res -i ErgoEmacs.rc
	gcc -s -o ErgoEmacs.exe ErgoEmacs.o ErgoEmacs.res -mwindows
	rm ErgoEmacs.o ErgoEmacs.res
