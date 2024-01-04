OBJS := main.o eigenvalues.o dispmodule.o
PROG := qr
FLAGS := -march=native -mtune=native -O1
COMPILER := gfortran $(FLAGS)

all: $(PROG)
	@./$(PROG)

test: .PHONY
	$(COMPILER) main.test.f90 -o test
	./test

$(PROG): $(OBJS)
	$(COMPILER) -o $@ $^

$(OBJS): %.o: %.f90
	$(COMPILER) -c -o $@ $<

main.o: dispmodule.o eigenvalues.o

eigenvalues.o: dispmodule.o

clean:
	rm -rf $(PROG) test *.o *.mod

dist-clean: clean
	rm -rf *.txt

.PHONY:
