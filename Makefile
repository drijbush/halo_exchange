PROG =	halo

SRCS =	constants.f90 halo_exchange_module_3d.f90 test.f90

OBJS =	constants.o halo_exchange_module_3d.o test.o

LIBS =-lblacs-openmpi -lblacsF77init-openmpi -lscalapack-openmpi -llapack -lblas	

CC = cc
CFLAGS = -O
FC = f77
FFLAGS = -O
F90 = mpif90
F90FLAGS = -O -g -std=f2008 -Wall -Wextra -Wimplicit-interface -Wuse-without-only -fcheck=all -finit-real=snan
LDFLAGS = 

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

halo_exchange_module_3d.o: constants.o
test.o: 
