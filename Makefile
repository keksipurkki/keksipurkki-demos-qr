OBJS := main.o eigenvalues.o dispmodule.o
PROG := qr
FLAGS := -march=native -mtune=native -O1
COMPILER := gfortran $(FLAGS)

all: $(PROG)
	@./$(PROG)

$(PROG): $(OBJS)
	$(COMPILER) -o $@ $^

$(OBJS): %.o: %.f90
	$(COMPILER) -c -o $@ $<

main.o: dispmodule.o eigenvalues.o

eigenvalues.o: dispmodule.o

clean:
	rm -rf $(PROG) *.o *.mod

dist-clean: clean

.PHONY:
