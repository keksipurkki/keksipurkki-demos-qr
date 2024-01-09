DEBUG :=
SIZE := 10
OBJS := main.o eigenvalues.o dispmodule.o utils.o
PROG := qr
LIBS := -framework Accelerate
FLAGS := -march=native -fexternal-blas -ffree-form -fimplicit-none -fbounds-check -O1
COMPILER := gfortran

ifdef DEBUG
	FLAGS += -D_DEBUG
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

test.out: dispmodule.o eigenvalues.o eigenvalues.test.o
	@$(COMPILER) $(LIBS) $(FLAGS) $^ -o $@

$(PROG): $(OBJS)
	$(COMPILER) $(LIBS) $(FLAGS) -o $@ $^

$(OBJS): %.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

%.o: %.F
	$(COMPILER) $(FLAGS) -c -o $@ $<

main.o: dispmodule.o eigenvalues.o utils.o

eigenvalues.o: dispmodule.o

eigenvalues.test.o: eigenvalues.o

clean:
	rm -rf $(PROG) *.out *.o *.mod

dist-clean: clean
	rm -rf *.txt

.PHONY: test.out scratch.out
