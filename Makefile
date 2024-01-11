.EXTRA_PREREQS:= $(abspath $(lastword $(MAKEFILE_LIST)))

DEBUG := 1
SIZE := 10
OBJS := eigenvalues.o dispmodule.o utils.o benchmarks.o
PROG := qr
LIBS := -framework Accelerate
FLAGS := -std=gnu -fall-intrinsics -fexternal-blas -ffree-form -fimplicit-none
COMPILER := gfortran

# Make IEEE-754 violations fatal
FLAGS += -ffpe-trap=invalid,zero,overflow,underflow

ifdef DEBUG
	FLAGS += -Wall -D_DEBUG -gdwarf-4 -g -static-libgfortran -Og -fcheck=all -fbacktrace
else
	FLAGS += -O3 -march=native
endif

all: test $(PROG) input.nml

input.nml:
	cat default_input.nml > input.nml

scratch: scratch.out
	./$@.out

scratch.out: dispmodule.o scratch.F
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

test: test.out
	./$@.out

test.out: $(OBJS) eigenvalues.test.F
	$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

$(PROG): $(OBJS) main.F
	$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@
	./$(PROG)

$(OBJS): %.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

%.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

eigenvalues.o: dispmodule.o utils.o

clean:
	rm -rf $(PROG) *.out *.o *.mod *.smod *.dSYM

dist-clean: clean
	rm -rf *.txt

.PHONY: scratch.out test.out qr
