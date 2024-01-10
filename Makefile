.EXTRA_PREREQS:= $(abspath $(lastword $(MAKEFILE_LIST)))

DEBUG := 1
SIZE := 10
OBJS := eigenvalues.o dispmodule.o utils.o
PROG := qr
LIBS := -framework Accelerate
FLAGS := -fexternal-blas -ffree-form -fimplicit-none
COMPILER := gfortran

ifdef DEBUG
	FLAGS += -Wall -D_DEBUG -g -Og -fcheck=all -fbacktrace
endif

all: test $(PROG) input.nml
	./$(PROG)

input.nml:
	cat default_input.nml > input.nml

scratch: scratch.out .PHONY
	./$@.out

scratch.out: scratch.o dispmodule.o
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

test: test.out
	./$@.out

test.out: $(OBJS) eigenvalues.test.o
	$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

$(PROG): main.o $(OBJS)
	$(COMPILER) $(LIBS) $(FLAGS) -o $@ $^

$(OBJS): %.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

%.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

eigenvalues.o: dispmodule.o utils.o

clean:
	rm -rf $(PROG) *.out *.o *.mod

dist-clean: clean
	rm -rf *.txt

.PHONY: test.out scratch.out
