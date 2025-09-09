F_FLAGS = -Wall -Wextra -pedantic -Wconversion -Wshadow -Wno-unused-function -Wno-compare-reals -std=f2018
C_FLAGS = -Wall -Wextra -pedantic -Wconversion -Wshadow -Wno-unused-function -std=c99
CXX_FLAGS = -Wall -Wextra -pedantic -Wconversion -Wshadow -std=c++23

DEBUG ?= 0

ifeq (${DEBUG}, 1)
	F_FLAGS += -O0 -g -fcheck=all
	C_FLAGS += -O0 -g
	CXX_FLAGS += -O0 -g
else
	F_FLAGS += -march=native -O3 -ffast-math -fstack-arrays
	C_FLAGS += -march=native -O3 -ffast-math
	CXX_FLAGS += -march=native -O3 -ffast-math
endif

SDL_INC = -I/opt/homebrew/include/SDL2
SDL_MOD = -J${HOME}/opt/fortran-sdl2
SDL_LIB = -L/opt/homebrew/lib -lSDL2 -lSDL2_ttf
SDL-FORTRAN_LIB = -L${HOME}/opt/fortran-sdl2 -lfortran-sdl2 -lfortran-sdl2_ttf

IMGUI_DIR = ./thirdparty/imgui
IMGUI_INC = -I${IMGUI_DIR} -I${IMGUI_DIR}/backends
IMGUI_SRC = ${IMGUI_DIR}/imgui.cpp ${IMGUI_DIR}/imgui_draw.cpp ${IMGUI_DIR}/imgui_tables.cpp ${IMGUI_DIR}/imgui_widgets.cpp
IMGUI_SRC += ${IMGUI_DIR}/backends/imgui_impl_sdl2.cpp ${IMGUI_DIR}/backends/imgui_impl_sdlrenderer2.cpp
IMGUI_OBJ = ${addprefix build/, ${notdir ${IMGUI_SRC:.cpp=.o}}}

IGOR_INC = -I${IGOR_DIR}

MCPC2: build src/MCPC2.cpp build/libimgui.a
	${CXX} ${CXX_FLAGS} ${SDL_INC} ${IMGUI_INC} ${IGOR_INC} -o $@ src/MCPC2.cpp -L./build -limgui ${SDL_LIB}

build/libimgui.a: ${IMGUI_OBJ}
	${AR} -rsc $@ ${IMGUI_OBJ}

build/%.o: ${IMGUI_DIR}/%.cpp
	${CXX} ${CXX_FLAGS} ${SDL_INC} ${IMGUI_INC} -c -o $@ $<

build/%.o: ${IMGUI_DIR}/backends/%.cpp
	${CXX} ${CXX_FLAGS} ${SDL_INC} ${IMGUI_INC} -c -o $@ $<

# ==================================================================================================

MCPC: build/MCPC.o build/render_text.o
	${FC} ${F_FLAGS} ${SDL_MOD} -o $@ $^ ${SDL_LIB} ${SDL-FORTRAN_LIB}

build/MCPC.o: src/MCPC.f90 build
	${FC} ${F_FLAGS} ${SDL_MOD} -c -o $@ $<

build/render_text.o: src/render_text.c build
	${CC} ${C_FLAGS} ${SDL_INC} -c -o $@ $<

build:
	mkdir -p build

clean:
	rm -fr MCPC MCPC2 *.dSYM build/

.PHONY: clean
