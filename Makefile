FC := nvfortran
FFLAGS := -O3 -mp=gpu -Minfo=accel

# Source files
SRC := TEST_CONTENT_FUNCTIONS.f90 TEST_TIME_FUNCTIONS.f90 main_program.f90

# Object files
OBJ := $(SRC:.f90=.o)

# Executable
EXECUTABLE := program

# Default target with "clean" as a dependency
all: clean $(EXECUTABLE)

# Compile Fortran source files to object files
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Link object files to create the executable
$(EXECUTABLE): $(OBJ)
	$(FC) $(FFLAGS) $(OBJ) -o $@ -lm

# Clean up generated files, excluding the executable
clean:
	@echo "Removing files: $(OBJ) *.mod"
	rm -f $(OBJ) *.mod

