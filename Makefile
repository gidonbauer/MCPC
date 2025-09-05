F_FLAGS = -Wall -Wextra -pedantic -Wconversion -Wshadow -Wno-unused-function -Wno-compare-reals -std=f2018
C_FLAGS = -Wall -Wextra -pedantic -Wconversion -Wshadow -Wno-unused-function -std=c99

DEBUG ?= 0

ifeq (${DEBUG}, 1)
	F_FLAGS += -O0 -g -fcheck=all
	C_FLAGS += -O0 -g
else
	F_FLAGS += -march=native -O3 -ffast-math -fstack-arrays
	C_FLAGS += -march=native -O3 -ffast-math
endif

SDL_INC = -I/opt/homebrew/include/SDL2
SDL_MOD = -J${HOME}/opt/fortran-sdl2
SDL_LIB = -L/opt/homebrew/lib -lSDL2 -lSDL2_ttf
SDL_LIB += -L${HOME}/opt/fortran-sdl2 -lfortran-sdl2 -lfortran-sdl2_ttf

MCPC: build/MCPC.o build/render_text.o
	${FC} ${F_FLAGS} ${SDL_MOD} -o $@ $^ ${SDL_LIB}

build/MCPC.o: src/MCPC.f90 build
	${FC} ${F_FLAGS} ${SDL_MOD} -c -o $@ $<

build/render_text.o: src/render_text.c build
	${CC} ${C_FLAGS} ${SDL_INC} -c -o $@ $<

build:
	mkdir -p build

clean:
	rm -fr MCPC *.dSYM build/

.PHONY: clean
