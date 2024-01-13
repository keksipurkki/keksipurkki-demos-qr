.EXTRA_PREREQS:= $(abspath $(lastword $(MAKEFILE_LIST)))

DEBUG := 1
SIZE := 10
OBJS := eigenvalues.o dispmodule.o utils.o benchmarks.o
PROG := qr
LIBS := -framework Accelerate
FLAGS := -std=gnu -fall-intrinsics -fexternal-blas -ffree-form -fimplicit-none
COMPILER := gfortran

ifdef DEBUG
	FLAGS += -D_DEBUG -gdwarf-4 -g -static-libgfortran -Og -fcheck=all -fbacktrace
else
	FLAGS += -O3 -march=native
endif

ifdef DEBUG
	FLAGS += -Wall -Wno-maybe-uninitialized
endif

all: test input.nml $(PROG)

input.nml:
	cat default_input.nml > input.nml

scratch: scratch.out
	./$@.out

scratch.out: dispmodule.o utils.o scratch.F
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

test: test.out
	./$@.out

test.out: $(OBJS) eigenvalues.test.F
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

$(PROG): $(OBJS) main.F
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@
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
