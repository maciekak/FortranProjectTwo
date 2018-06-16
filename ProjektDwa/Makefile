F90=ifort

FLAGS = -module . -std08 -fpp -O2

build:
	$(F90) test.f90 MULT.f90 ProjektDwa.f90 -o main $(FLAGS)

test: build ./main