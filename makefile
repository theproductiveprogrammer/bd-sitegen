bd-gen.exe: bd-gen.scm
	/usr/bin/chicken bd-gen.scm -output-file bd-gen.c -quiet -uses regex,posix,srfi-1,srfi-13
	gcc bd-gen.c -o bd-gen.o -c -fno-strict-aliasing -DHAVE_CHICKEN_CONFIG_H -Os
	rm bd-gen.c
	gcc bd-gen.o -o bd-gen -L/usr/bin  -Wl,-R/usr/bin -lchicken -lm
	rm bd-gen.o

clean:
	rm bd-gen.exe

allclean: clean
