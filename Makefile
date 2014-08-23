
CCX = g++
CC = gcc
TARGET = vba-sdl-h
BININSTALLPATH = /usr/bin
ETC = /etc

INCLUDES = -I/usr/include/libpng12 -I/usr/include/SDL -Isrc

LIBS = -L/usr/lib -lSDL -lpthread -lreadline -lpng12 -lz -lminizip

CXXFLAGS = -Wno-write-strings -DBKPT_SUPPORT -DSYSCONFDIR=\"/etc\" \
-DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" \
-DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE=\"VisualBoyAdvance\" \
-DVERSION=\"1.7.2-H\" -DYYTEXT_POINTER=1 -DHAVE_LIBZ=1 -DHAVE_LIBPNG=1 \
-DHAVE_LIBPTHREAD=1 -DHAVE_LIBREADLINE=1 -DSTDC_HEADERS=1 \
-DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 \
-DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 \
-DHAVE_UNISTD_H=1 -DHAVE_MALLOC_H=1 -DHAVE_STRINGS_H=1 -DHAVE_UNISTD_H=1 \
-DHAVE_ARPA_INET_H=1 -DHAVE_NETINET_IN_H=1 -I.  -I../../src -DSDL \
-fno-exceptions -D_GNU_SOURCE=1 -D_REENTRANT -g -O2 -DC_CORE -DPROFILING \
-DDEV_VERSION -DBKPT_SUPPORT

OBJECTS = ./src/motionblur.o ./src/agbprint.o \
./src/Mode2.o ./src/Gfx.o ./src/interframe.o ./src/sdl/debugger.o \
./src/sdl/debugger-expr-yacc.o ./src/sdl/SDL.o \
./src/sdl/debugger-expr-lex.o ./src/GBA.o ./src/Flash.o ./src/2xSaI.o \
./src/prof/prof.o ./src/Sram.o ./src/Mode1.o ./src/Util.o ./src/Mode5.o \
./src/expr.o ./src/getopt.o ./src/Sound.o ./src/Mode0.o \
./src/Mode3.o ./src/RTC.o ./src/armdis.o ./src/hq2x.o ./src/exprNode.o \
./src/Globals.o ./src/expr-lex.o ./src/scanline.o ./src/remote.o \
./src/admame.o ./src/simple2x.o ./src/gb/gbDis.o ./src/gb/gbSGB.o \
./src/gb/gbGfx.o ./src/gb/gbCheats.o ./src/gb/gbGlobals.o \
./src/gb/gbSound.o ./src/gb/gbPrinter.o ./src/gb/GB.o ./src/gb/gbMemory.o \
./src/Text.o ./src/Mode4.o ./src/Cheats.o ./src/CheatSearch.o ./src/elf.o \
./src/pixel.o ./src/bios.o ./src/getopt1.o ./src/bilinear.o ./src/EEprom.o \
./src/memgzio.o
# ./src/memgzio.o ./src/sdl/TestEmu.o ./src/unzip.o \

%.o : %.cpp
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@

$(TARGET): $(OBJECTS)
	g++ $(INCLUDES) $(LIBS) $^ -o $@

clean:
	rm -f $(OBJECTS) $(TARGET)

install:
	install "$(TARGET)" "$(BININSTALLPATH)"
	install "src/VisualBoyAdvance.cfg" "$(ETC)"

uninstall:
	rm -f "$(BININSTALLPATH)/$(TARGET)"
	rm -f "$(ETC)/VisualBoyAdvance.cfg"

