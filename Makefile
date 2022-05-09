F90C = ifort
OPTS = -O0 -traceback -CB -g #-pg # -g -a #-CB
# OPTS = -O2

OBJECTS = kinds.o graphft.o mathsops.o

%.o: %.f90
	$(F90C) $(OPTS) $< -c

graph_test.x: graph_test.f90 $(OBJECTS)
	$(F90C) $(OPTS) $^ -o $@ $(LIBS)

graphft.o: kinds.o mathsops.o
kinds.o:

clean:
	rm -f *.mod *.o *.x
