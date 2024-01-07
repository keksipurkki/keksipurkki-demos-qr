DEBUG :=
SIZE := 10
OBJS := main.o eigenvalues.o dispmodule.o utils.o
PROG := qr
LIBS := -framework Accelerate
FLAGS := -fexternal-blas -march=native -ffree-form -fimplicit-none -fbounds-check -O1
COMPILER := gfortran

ifdef DEBUG
	FLAGS += -D_DEBUG
endif

all: $(PROG) input.nml
	@./$(PROG)

input.nml:
	cat default_input.nml > input.nml

test: .PHONY
	$(COMPILER) $(FLAGS) -c scratch.F
	$(COMPILER) $(FLAGS) scratch.o dispmodule.o -o test
	./test

$(PROG): $(OBJS)
	$(COMPILER) $(LIBS) $(FLAGS) -o $@ $^

$(OBJS): %.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

main.o: dispmodule.o eigenvalues.o utils.o

eigenvalues.o: dispmodule.o

clean:
	rm -rf $(PROG) test *.o *.mod

dist-clean: clean
	rm -rf *.txt

.PHONY:
