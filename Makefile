.EXTRA_PREREQS:= $(abspath $(lastword $(MAKEFILE_LIST)))

DEBUG := 1
SIZE := 10
OBJS := eigenvalues.o dispmodule.o utils.o
PROG := qr
LIBS := -framework Accelerate
FLAGS := -fexternal-blas -ffree-form -fimplicit-none
COMPILER := gfortran

ifdef DEBUG
	FLAGS += -Wall -D_DEBUG -gdwarf-4 -g -static-libgfortran -Og -fcheck=all -fbacktrace
else
	FLAGS += -O3
endif

all: test $(PROG) input.nml
	./$(PROG)

input.nml:
	cat default_input.nml > input.nml

scratch: scratch.out .PHONY
	./$@.out

scratch.out: dispmodule.o
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@ $@.F

test: test.out
	./$@.out

test.out: $(OBJS)
	$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@ eigenvalues.test.F

$(PROG): $(OBJS)
	$(COMPILER) $(LIBS) $(FLAGS) -o $@ $^ main.F

$(OBJS): %.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

%.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

eigenvalues.o: dispmodule.o utils.o

clean:
	rm -rf $(PROG) *.out *.o *.mod *.smod *.dSYM

dist-clean: clean
	rm -rf *.txt

.PHONY: test.out scratch.out
