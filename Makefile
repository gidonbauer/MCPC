F_FLAGS = -Wall -Wextra -pedantic -Wconversion -Wshadow -std=f2018

DEBUG ?= 0

ifeq (${DEBUG}, 1)
	F_FLAGS += -O0 -g -fcheck=all
else
	F_FLAGS += -march=native -O3 -ffast-math -fstack-arrays
endif

SDL_MOD = -J${HOME}/opt/fortran-sdl2
SDL_LIB = -L/opt/homebrew/lib -lSDL2 -L${HOME}/opt/fortran-sdl2 -lfortran-sdl2

MCPC: MCPC.f90
	${FC} ${F_FLAGS} ${SDL_MOD} -o $@ $< ${SDL_LIB}

clean:
	rm -fr MCPC MCPC.dSYM

.PHONY: clean
