SIZE := 10
OBJS := main.o eigenvalues.o dispmodule.o utils.o
PROG := qr
FLAGS := -march=native -fbounds-check -O1 -D_DEBUG -D_SIZE=$(SIZE)
COMPILER := gfortran $(FLAGS)

all: $(PROG)
	@./$(PROG)
	./lambda.py

test: .PHONY
	$(COMPILER) -c scratch.F90
	$(COMPILER) scratch.o dispmodule.o -o test
	./test

$(PROG): $(OBJS)
	$(COMPILER) -o $@ $^

$(OBJS): %.o: %.F90
	$(COMPILER) -c -o $@ $<

main.o: dispmodule.o eigenvalues.o utils.o

eigenvalues.o: dispmodule.o

clean:
	rm -rf $(PROG) test *.o *.mod

dist-clean: clean
	rm -rf *.txt

.PHONY:
