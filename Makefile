FC := gfortran
FFLAGS := -O2 

# Source files
SRC := TEST_CONTENT_FUNCTIONS.f90 TEST_TIME_FUNCTIONS.f90 main_program.f90 

# Object files
OBJ := $(SRC:.f90=.o)

# Executable
EXECUTABLE := program

# Default target
all: $(EXECUTABLE)

# Compile Fortran source files to object files
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Link object files to create the executable
$(EXECUTABLE): $(OBJ)
	$(FC) $(FFLAGS) $(OBJ) -o $@

# Clean up generated files
clean:
	del $(OBJ) $(EXECUTABLE)
	del $(OBJ) $(EXECUTABLE) *.mod

# Fortran code error solution
# Ensure that the module is compiled before the main program
# main_program.o: XYCALCZEFF.o

# Implicit rule to compile Fortran source files to object files
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@
