.EXTRA_PREREQS:= $(abspath $(lastword $(MAKEFILE_LIST)))

DEBUG := 1
SIZE := 10
OBJS := eigenvalues.o dispmodule.o utils.o benchmarks.o
PROG := qr
LIBS := -framework Accelerate
FLAGS := -std=gnu -fall-intrinsics -ffree-form -fimplicit-none
COMPILER := gfortran
PERF_FLAGS := -std=gnu -ffree-form -fimplicit-none -O3 -march=native -mtune=native -fexternal-blas

ifdef DEBUG
	FLAGS += -D_DEBUG -gdwarf-4 -g -static-libgfortran -Og -fcheck=all -fbacktrace
endif

ifdef DEBUG
	FLAGS += -Wall -Wno-maybe-uninitialized
endif

all: test input.nml $(PROG)

input.nml:
	cat default_input.nml > input.nml

perf: perf.out
	./$@.out

perf.out: dlahqr.o dispmodule.o utils.o benchmarks.o perf.F
	$(COMPILER) $(PERF_FLAGS) -c eigenvalues.F -o eigenvalues.o
	$(COMPILER) $(LIBS) $(PERF_FLAGS) eigenvalues.o $^ -o $@

scratch: scratch.out
	./$@.out

scratch.out: dispmodule.o utils.o scratch.F
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

test: test.out
	./$@.out

test.out: $(OBJS) eigenvalues.test.F
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

dlahqr.o: dlahqr.F
	$(COMPILER) -c -std=legacy dlahqr.F -o dlahqr.o

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

.PHONY: scratch.out test.out qr perf.out
