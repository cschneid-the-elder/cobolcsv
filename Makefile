
CFLAGS=-Wall -Wpossible-overlap -Wimplicit-define -Wcolumn-overflow -Wpossible-truncate -Wunreachable -fdump=ALL -ftraceall

all: CSVPARSE.so EXAMPLE1

.PHONY: all

CSVPARSE.so: src/CSVPARSE.cbl src/CSVOPTS src/CSVPARMS src/CSVRC
	echo `date` $< >>build.log
	cobc $(CFLAGS) -I src -X -t CSVPARSE.lst -m $<

EXAMPLE1: src/EXAMPLE1.cbl src/CSVOPTS src/CSVPARMS src/CSVRC
	echo `date` $< >>build.log
	cobc $(CFLAGS) -I src -X -t $@.lst -x src/$@.cbl

test:
	echo `date` $@ >>build.log
	./EXAMPLE1 testdata/example1 , R F
	./EXAMPLE1 testdata/example2 , R T
	./EXAMPLE1 testdata/example3 , r t
	./EXAMPLE1 testdata/example4 , U F
	./EXAMPLE1 testdata/example5 : U F

