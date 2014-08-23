// VisualBoyAdvance - Nintendo Gameboy/GameboyAdvance (TM) emulator.
// Copyright (C) 1999-2003 Forgotten
// Copyright (C) 2004 Forgotten and the VBA development team

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or(at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// VBA-H additions by kenobi and Labmaster

// Includes search functions by DevZ aka F-zero

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "GBA.h"
#include "Port.h"
#include "armdis.h"
#include "elf.h"
#include "exprNode.h"
#ifdef HAVE_LIBREADLINE
// work around for Function defined in elf.h
#if !defined (_FUNCTION_DEF)
#  define _FUNCTION_DEF
#endif
#include <readline/readline.h>
#include <readline/history.h>
#endif
extern bool debugger;
extern int emulating;

extern struct EmulatedSystem emulator;

extern void sdlWriteState(int num);
extern void sdlReadState(int num);

extern bool dexp_eval(char *, u32*);
extern void dexp_setVar(char *, u32);
extern void dexp_listVars();
extern void dexp_saveVars(char *);
extern void dexp_loadVars(char *);

#define debuggerReadMemory(addr) \
  READ32LE((&map[(addr)>>24].address[(addr) & map[(addr)>>24].mask]))

#define debuggerReadHalfWord(addr) \
  READ16LE((&map[(addr)>>24].address[(addr) & map[(addr)>>24].mask]))

#define debuggerReadByte(addr) \
  map[(addr)>>24].address[(addr) & map[(addr)>>24].mask]

#define debuggerWriteMemory(addr, value) \
  WRITE32LE(&map[(addr)>>24].address[(addr) & map[(addr)>>24].mask], value)

#define debuggerWriteHalfWord(addr, value) \
  WRITE16LE(&map[(addr)>>24].address[(addr) & map[(addr)>>24].mask], value)

#define debuggerWriteByte(addr, value) \
  map[(addr)>>24].address[(addr) & map[(addr)>>24].mask] = (value)

#ifdef __GNUC__
#ifndef HAVE_STRLWR
char * strlwr(char *str)
{
  char *s;
  for(s=str; *s; s++)
    *s = tolower(*s);
  return str;
}
#endif
#endif
#ifdef _MSC_VER
#define strlwr _strlwr
#endif

struct breakpointInfo {
  u32 address;
  u32 value;
  int size;
};

struct DebuggerCommand {
  char *name;
  void (*function)(int,char **);
  char *help;
  char *syntax;
};

void debuggerContinueAfterBreakpoint();

void debuggerHelp(int,char **);
void debuggerNext(int,char **);
void debuggerContinue(int, char **);
void debuggerRegisters(int, char **);
void debuggerBreak(int, char **);
void debuggerBreakDelete(int, char **);
void debuggerBreakList(int, char **);
void debuggerBreakArm(int, char **);
void debuggerBreakWriteClear(int, char **);
void debuggerBreakThumb(int, char **);
void debuggerBreakWrite(int, char **);
void debuggerDebug(int, char **);
void debuggerDisassemble(int, char **);
void debuggerDisassembleNear(int, char **);
void debuggerDisassembleArm(int, char **);
void debuggerDisassembleThumb(int, char **);
void debuggerEditByte(int, char **);
void debuggerEditHalfWord(int, char **);
void debuggerEdit(int, char **);
void debuggerEval(int, char **);
void debuggerIo(int, char **);
void debuggerLocals(int, char **);
void debuggerMemoryByte(int, char **);
void debuggerMemoryHalfWord(int, char **);
void debuggerMemory(int, char **);
void debuggerPrint(int, char **);
void debuggerQuit(int, char **);
void debuggerSetRadix(int, char **);
void debuggerSymbols(int, char **);
void debuggerVerbose(int, char **);
void debuggerWhere(int, char **);

void debuggerReadState(int, char **);
void debuggerWriteState(int, char **);
void debuggerDontBreak(int, char **);
void debuggerDontBreakClear(int, char **);
void debuggerDumpLoad(int, char**);
void debuggerDumpSave(int, char**);
void debuggerEditRegister(int, char **);
void debuggerTrace(int, char **);
void debuggerVar(int, char **);

bool debuggerInDB(u32 address);
void debuggerBreakRead(int, char **);
void debuggerBreakReadClear(int, char **);

void debuggerRunTo(int, char **);
void debuggerRunToArm(int, char **);
void debuggerRunToThumb(int, char **);

void debuggerDoSearch();
unsigned int AddressToGBA(u8* mem);

void debuggerFindText(int, char **);
void debuggerFindHex(int, char **);
void debuggerFindResume(int, char **);

bool dexp_eval(char *, u32 *);


DebuggerCommand debuggerCommands[] = {
  { "?", debuggerHelp,        "Shows this help information. Type ? <command> for command help", "[<command>]" },
  { "ba", debuggerBreakArm,   "Adds an ARM breakpoint", "<address>" },
  { "bd", debuggerBreakDelete,"Deletes a breakpoint", "<number>" },
  { "bl", debuggerBreakList,  "Lists breakpoints" },
  { "bpr", debuggerBreakRead, "Break on read", "<address> <size>" },
  { "bprc", debuggerBreakReadClear, "Clear break on read", NULL },
  { "bpw", debuggerBreakWrite, "Break on write", "<address> <size>" },
  { "bpwc", debuggerBreakWriteClear, "Clear break on write", NULL },
  { "bt", debuggerBreakThumb, "Adds a THUMB breakpoint", "<address>" },
  { "c", debuggerContinue,    "Continues execution" , NULL },
  { "d", debuggerDisassemble, "Disassembles instructions", "[<address> [<number>]]" },
  { "da", debuggerDisassembleArm, "Disassembles ARM instructions", "[{address} [{number}]]" },
  { "db", debuggerDontBreak, "Don't break at the following address.", "[<address> [<number>]]" },
  { "dbc", debuggerDontBreakClear, "Clear the Don't Break list.", NULL },
  { "dload",debuggerDumpLoad, "Load raw data dump from file","<file> {address}"},
  { "dsave",debuggerDumpSave, "Dump raw data to file","<file> {address} {size}"},  
  { "dn", debuggerDisassembleNear, "Disassembles instructions near PC", "[<number>]" },
  { "dt", debuggerDisassembleThumb, "Disassembles Thumb instructions", "[{address} [{number}]]" },
  { "eb", debuggerEditByte,   "Modify memory location (byte)", "{address} {value}" },
  { "eh", debuggerEditHalfWord,"Modify memory location (half-word)","{address} {value}" },
  { "er", debuggerEditRegister,       "Modify register", "<register number> {value}" },  
  { "eval", debuggerEval, "Evaluate expression", "<expression>"},
  { "ew", debuggerEdit,       "Modify memory location (word)", "{address} {value}" },
  { "ft",      debuggerFindText,         "Search memory for ASCII-string.", "<start> [<max-result>] <string>" },
  { "fh",      debuggerFindHex,          "Search memory for hex-string.", "<start> [<max-result>] <hex-string>" },
  { "fr",      debuggerFindResume,       "Resume current search.", "[<max-result>]" },
  { "h", debuggerHelp,        "Shows this help information. Type h <command> for command help", "[<command>]" },
  { "io", debuggerIo,         "Show I/O registers status", "[video|video2|dma|timer|misc]" },
  { "load", debuggerReadState,	"Loads a Fx type savegame", "<number>" },  
  { "mb", debuggerMemoryByte, "Shows memory contents (bytes)", "{address}" },
  { "mh", debuggerMemoryHalfWord, "Shows memory contents (half-words)", "{address}"},
  { "mw", debuggerMemory,     "Shows memory contents (words)", "{address}" },
  { "n", debuggerNext,        "Executes the next instruction", "[<count>]" },
  { "q", debuggerQuit,        "Quits the emulator", NULL },
  { "r", debuggerRegisters,   "Shows ARM registers", NULL },
  { "rt", debuggerRunTo, "Run to address", "{address}"},
  { "rta", debuggerRunToThumb, "Run to address (Thumb)", "{address}"},
  { "rtt", debuggerRunToArm, "Run to address (ARM)", "{address}"},
  { "save", debuggerWriteState,	"Creates a Fx type savegame", "<number>" },  
  { "sbreak", debuggerBreak,    "Adds a breakpoint on the given function", "<function>|<line>|<file:line>" },
  { "sradix", debuggerSetRadix,   "Sets the print radix", "<radix>" },
  { "sprint", debuggerPrint,   "Print the value of a expression (if known)", "[/x|/o|/d] <expression>" },
  { "ssymbols", debuggerSymbols, "List symbols", "[<symbol>]" },
#ifndef FINAL_VERSION
  { "strace", debuggerDebug,       "Sets the trace level", "<value>" },
#endif
#ifdef DEV_VERSION
  { "sverbose", debuggerVerbose,     "Change verbose setting", "<value>" },
#endif
  { "swhere", debuggerWhere,   "Shows call chain", NULL },
  { "trace", debuggerTrace, "Control tracer", "start|stop|file <file>"},
  { "var", debuggerVar, "Define variables", "<name> {variable}"},
  { NULL, NULL, NULL, NULL} // end marker
};

breakpointInfo debuggerBreakpointList[100];
#define NUMBEROFDB 1000
u32 debuggerNoBreakpointList[NUMBEROFDB];
breakpointInfo debuggerBkptRunTo;

int debuggerNumOfBreakpoints = 0;
int debuggerNumOfDontBreak = 0;
bool debuggerAtBreakpoint = false;
int debuggerBreakpointNumber = 0;
int debuggerRadix = 0;
bool debuggerTracing = false;
char debuggerTraceFilename[200];
FILE * debuggerTraceFile;
bool debuggerTraceReg = true;
bool debuggerTraceOnce = true;
bool debuggerTraceSkipped = false;
u32 debuggerTraceLast = 0;

#ifdef HAVE_LIBREADLINE
// A static variable for holding the current debugger line.
static char *line_read = (char *)NULL;
#endif

int debuggerContext = 1;

unsigned int SearchStart = 0xFFFFFFFF;
unsigned int SearchMaxMatches = 5;
u8 SearchData [64];		// It actually doesn't make much sense to search for more than 64 bytes, does it?
unsigned int SearchLength = 0;
unsigned int SearchResults;


void debuggerApplyBreakpoint(u32 address, int num, int size)
{
  if(size)
    debuggerWriteMemory(address, (u32)(0xe1200070 | 
                                       (num & 0xf) | 
                                       ((num<<4)&0xf0)));
  else
    debuggerWriteHalfWord(address, 
                          (u16)(0xbe00 | num));
}

void debuggerDisableBreakpoints()
{
  for(int i = 0; i < debuggerNumOfBreakpoints; i++) {
    if(debuggerBreakpointList[i].size)
      debuggerWriteMemory(debuggerBreakpointList[i].address,
                          debuggerBreakpointList[i].value);
    else
      debuggerWriteHalfWord(debuggerBreakpointList[i].address,
                            debuggerBreakpointList[i].value);      
  }

  if (debuggerBkptRunTo.address) {
    if(debuggerBkptRunTo.size)
      debuggerWriteMemory(debuggerBkptRunTo.address,
			  debuggerBkptRunTo.value);
    else
      debuggerWriteHalfWord(debuggerBkptRunTo.address,
			    debuggerBkptRunTo.value);
  }
}

void debuggerEnableBreakpoints(bool skipPC)
{
  for(int i = 0; i < debuggerNumOfBreakpoints; i++) {
    if(debuggerBreakpointList[i].address == armNextPC && skipPC)
      continue;

    debuggerApplyBreakpoint(debuggerBreakpointList[i].address,
                            i,
                            debuggerBreakpointList[i].size);
  }

  if (debuggerBkptRunTo.address) {
    debuggerApplyBreakpoint(debuggerBkptRunTo.address, 255,
			    debuggerBkptRunTo.size);
  }
}

void debuggerUsage(char *cmd)
{
  for(int i = 0; ; i++) {
    if(debuggerCommands[i].name) {
      if(!strcmp(debuggerCommands[i].name, cmd)) {
        printf("%s %s\t%s\n", 
               debuggerCommands[i].name, 
               debuggerCommands[i].syntax ? debuggerCommands[i].syntax : "",
               debuggerCommands[i].help);
        break;
      }
    } else {
      printf("Unrecognized command '%s'.", cmd);
      break;
    }
  }  
}

void debuggerPrintBaseType(Type *t, u32 value, u32 location,
                           LocationType type,
                           int bitSize, int bitOffset)
{
  if(bitSize) {
    if(bitOffset)
      value >>= ((t->size*8)-bitOffset-bitSize);
    value &= (1 << bitSize)-1;
  } else {
    if(t->size == 2)
      value &= 0xFFFF;
    else if(t->size == 1)
      value &= 0xFF;
  }

  if(t->size == 8) {
    u64 value = 0;
    if(type == LOCATION_memory) {
      value = debuggerReadMemory(location) |
        ((u64)debuggerReadMemory(location+4)<<32);
    } else if(type == LOCATION_register) {
      value = reg[location].I | ((u64)reg[location+1].I << 32);
    }
    switch(t->encoding) {
    case DW_ATE_signed:
      switch(debuggerRadix) {
      case 0:
        printf("%lld", value);
        break;
      case 1:
        printf("0x%llx", value);
        break;
      case 2:
        printf("0%llo", value);
        break;
      }
      break;
    case DW_ATE_unsigned:
      switch(debuggerRadix) {
      case 0:
        printf("%llu", value);
        break;
      case 1:
        printf("0x%llx", value);
        break;
      case 2:
        printf("0%llo", value);
        break;
      }
      break;
    default:
      printf("Unknowing 64-bit encoding\n");
    }
    return;
  }
  
  switch(t->encoding) {
  case DW_ATE_boolean:
    if(value)
      printf("true");
    else
      printf("false");
    break;
  case DW_ATE_signed:
    switch(debuggerRadix) {
    case 0:
      printf("%d", value);
      break;
    case 1:
      printf("0x%x", value);
      break;
    case 2:
      printf("0%o", value);
      break;
    }
    break;
  case DW_ATE_unsigned:
  case DW_ATE_unsigned_char:
    switch(debuggerRadix) {
    case 0:
      printf("%u", value);
      break;
    case 1:
      printf("0x%x", value);
      break;
    case 2:
      printf("0%o", value);
      break;
    }
    break;
  default:
    printf("UNKNOWN BASE %d %08x", t->encoding, value);
  }
}

char *debuggerPrintType(Type *t)
{
  char buffer[1024];  
  static char buffer2[1024];
  
  if(t->type == TYPE_pointer) {
    if(t->pointer)
      strcpy(buffer, debuggerPrintType(t->pointer));
    else
      strcpy(buffer, "void");
    sprintf(buffer2, "%s *", buffer);
    return buffer2;
  } else if(t->type == TYPE_reference) {
    strcpy(buffer, debuggerPrintType(t->pointer));
    sprintf(buffer2, "%s &", buffer);
    return buffer2;    
  }
  return t->name;
}

void debuggerPrintValueInternal(Function *, Type *, ELFBlock *, int, int, u32);
void debuggerPrintValueInternal(Function *f, Type *t,
                                int bitSize, int bitOffset,
                                u32 objLocation, LocationType type);

u32 debuggerGetValue(u32 location, LocationType type)
{
  switch(type) {
  case LOCATION_memory:
    return debuggerReadMemory(location);
  case LOCATION_register:
    return reg[location].I;
  case LOCATION_value:
    return location;
  }
  return 0;
}

void debuggerPrintPointer(Type *t, u32 value)
{
  printf("(%s)0x%08x", debuggerPrintType(t), value);
}

void debuggerPrintReference(Type *t, u32 value)
{
  printf("(%s)0x%08x", debuggerPrintType(t), value);
}

void debuggerPrintFunction(Type *t, u32 value)
{
  printf("(%s)0x%08x", debuggerPrintType(t), value);
}

void debuggerPrintArray(Type *t, u32 value)
{
  // todo
  printf("(%s[])0x%08x", debuggerPrintType(t->array->type), value);
}

void debuggerPrintMember(Function *f,
                         Member *m,
                         u32 objLocation,
                         u32 location)
{
  int bitSize = m->bitSize;
  if(bitSize) {
    u32 value = 0;
    int off = m->bitOffset;
    int size = m->byteSize;
    u32 v = 0;
    if(size == 1)
      v = debuggerReadByte(location);
      else if(size == 2)
        v = debuggerReadHalfWord(location);
      else if(size == 4)
        v = debuggerReadMemory(location);
      
      while(bitSize) {
        int top = size*8 - off;
        int bot = top - bitSize;
        top--;
        if(bot >= 0) {
          value = (v >> (size*8 - bitSize - off)) & ((1 << bitSize)-1);
          bitSize = 0;
        } else {
          value |= (v & ((1 << top)-1)) << (bitSize - top);
          bitSize -= (top+1);
          location -= size;
          off = 0;
          if(size == 1)
            v = debuggerReadByte(location);
          else if(size == 2)
            v = debuggerReadHalfWord(location);
          else
            v = debuggerReadMemory(location);
        }
      }
      debuggerPrintBaseType(m->type, value, location, LOCATION_memory,
                            bitSize, 0);
    } else {
      debuggerPrintValueInternal(f, m->type, m->location, m->bitSize,
                                 m->bitOffset, objLocation);
    }  
}

void debuggerPrintStructure(Function *f, Type *t, u32 objLocation)
{
  printf("{");
  int count = t->structure->memberCount;
  int i = 0;
  while(i < count) {
    Member *m = &t->structure->members[i];
    printf("%s=", m->name);
    LocationType type;
    u32 location = elfDecodeLocation(f, m->location, &type, objLocation);
    debuggerPrintMember(f, m, objLocation, location);
    i++;
    if(i < count)
      printf(",");
  }
  printf("}");
}

void debuggerPrintUnion(Function *f, Type *t, u32 objLocation)
{
  // todo
  printf("{");
  int count = t->structure->memberCount;
  int i = 0;
  while(i < count) {
    Member *m = &t->structure->members[i];
    printf("%s=", m->name);
    debuggerPrintMember(f, m, objLocation, 0);
    i++;
    if(i < count)
      printf(",");
  }
  printf("}");
}

void debuggerPrintEnum(Type *t, u32 value)
{
  int i;
  for(i = 0; i < t->enumeration->count; i++) {
    EnumMember *m = (EnumMember *)&t->enumeration->members[i];
    if(value == m->value) {
      printf(m->name);
      return;
    }
  }
  printf("(UNKNOWN VALUE) %d", value);
}

void debuggerPrintValueInternal(Function *f, Type *t,
                                int bitSize, int bitOffset,
                                u32 objLocation, LocationType type)
{
  u32 value = debuggerGetValue(objLocation, type);
  if(!t) {
    printf("void");
    return;
  }
  switch(t->type) {
  case TYPE_base:
    debuggerPrintBaseType(t, value, objLocation, type, bitSize, bitOffset);
    break;
  case TYPE_pointer:
    debuggerPrintPointer(t, value);
    break;
  case TYPE_reference:
    debuggerPrintReference(t, value);
    break;
  case TYPE_function:
    debuggerPrintFunction(t, value);
    break;
  case TYPE_array:
    debuggerPrintArray(t, objLocation);
    break;
  case TYPE_struct:
    debuggerPrintStructure(f, t, objLocation);
    break;
  case TYPE_union:
    debuggerPrintUnion(f, t, objLocation);
    break;
  case TYPE_enum:
    debuggerPrintEnum(t, value);
    break;
  default:
    printf("%08x", value);
    break;
  }  
}

void debuggerPrintValueInternal(Function *f, Type *t, ELFBlock *loc,
                                int bitSize, int bitOffset, u32 objLocation)
{
  LocationType type;  
  u32 location;
  if(loc) {
    if(objLocation)
      location = elfDecodeLocation(f, loc, &type, objLocation);
    else
      location = elfDecodeLocation(f, loc,&type);
  } else {
    location = objLocation;
    type = LOCATION_memory;
  }

  debuggerPrintValueInternal(f, t, bitSize, bitOffset, location, type);
}

void debuggerPrintValue(Function *f, Object *o)
{
  debuggerPrintValueInternal(f, o->type, o->location, 0, 0, 0);
  
  printf("\n");
}

void debuggerSymbols(int argc, char **argv)
{
  int i = 0;
  u32 value;
  u32 size;
  int type;
  bool match = false;
  int matchSize = 0;
  char *matchStr = NULL;
  
  if(argc == 2) {
    match = true;
    matchSize = strlen(argv[1]);
    matchStr = argv[1];
  }
  printf("Symbol               Value    Size     Type   \n");
  printf("-------------------- -------  -------- -------\n");
  char *s = NULL;
  while((s = elfGetSymbol(i, &value, &size, &type))) {
    if(*s) {
      if(match) {
        if(strncmp(s, matchStr, matchSize) != 0) {
          i++;
          continue;
        }
      }
      char *ts = "?";
      switch(type) {
      case 2:
        ts = "ARM";
        break;
      case 0x0d:
        ts = "THUMB";
        break;
      case 1:
        ts = "DATA";
        break;
      }
      printf("%-20s %08x %08x %-7s\n",
             s, value, size, ts);
    }
    i++;
  }
}

void debuggerSetRadix(int argc, char **argv)
{
  if(argc != 2)
    debuggerUsage(argv[0]);
  else {
    int r = atoi(argv[1]);

    bool error = false;
    switch(r) {
    case 10:
      debuggerRadix = 0;
      break;
    case 8:
      debuggerRadix = 2;
      break;
    case 16:
      debuggerRadix = 1;
      break;
    default:
      error = true;
      printf("Unknown radix %d. Valid values are 8, 10 and 16.\n", r);
      break;
    }
    if(!error)
      printf("Radix set to %d\n", r);
  }
}

void debuggerPrint(int argc, char **argv)
{
  if(argc != 2 && argc != 3) {
    debuggerUsage(argv[0]);
  } else {
    u32 pc = armNextPC;
    Function *f = NULL;
    CompileUnit *u = NULL;
    
    elfGetCurrentFunction(pc,
                          &f, &u);

    int oldRadix = debuggerRadix;
    if(argc == 3) {
      if(argv[1][0] == '/') {
        if(argv[1][1] == 'x')
          debuggerRadix = 1;
        else if(argv[1][1] == 'o')
          debuggerRadix = 2;
        else if(argv[1][1] == 'd')
          debuggerRadix = 0;
        else {
          printf("Unknown format %c\n", argv[1][1]);
          return;
        }
      } else {
        printf("Unknown option %s\n", argv[1]);
        return;
      }
    } 
    
    char *s = argc == 2 ? argv[1] : argv[2];

    extern char *exprString;
    extern int exprCol;
    extern int yyparse();
    exprString = s;
    exprCol = 0;
    if(!yyparse()) {
      extern Node *result;
      if(result->resolve(result, f, u)) {
        if(result->member)
          debuggerPrintMember(f,
                              result->member,
                              result->objLocation,
                              result->location);
        else
          debuggerPrintValueInternal(f, result->type, 0, 0,
                                     result->location,
                                     result->locType);
        printf("\n");
      } else {
        printf("Error resolving expression\n");
      }
    } else {
      printf("Error parsing expression:\n");
      printf("%s\n", s);
      exprCol--;
      for(int i = 0; i < exprCol; i++)
        printf(" ");
      printf("^\n");
    }
    extern void exprCleanBuffer();
    exprCleanBuffer();
    exprNodeCleanUp();
    debuggerRadix = oldRadix;
  }
}

void debuggerHelp(int n, char **args)
{
  if(n == 2) {
    debuggerUsage(args[1]);
  } else {
    for(int i = 0; ; i++) {
      if(debuggerCommands[i].name) {
        printf("%-10s%s\n", debuggerCommands[i].name, debuggerCommands[i].help);
      } else
        break;
    }
  }
}

void debuggerDebug(int n, char **args)
{
  if(n == 2) {
    int v = 0;
    sscanf(args[1], "%d", &v);
    systemDebug = v;
    printf("Debug level set to %d\n", systemDebug);
  } else
    debuggerUsage(args[0]);      
}

void debuggerVerbose(int n, char **args)
{
  if(n == 2) {
    int v = 0;
    sscanf(args[1], "%d", &v);
    systemVerbose = v;
    printf("Verbose level set to %d\n", systemVerbose);
  } else
    debuggerUsage("verbose");    
}

void debuggerWhere(int n, char **args)
{
  void elfPrintCallChain(u32);
  elfPrintCallChain(armNextPC);
}

void debuggerLocals(int n, char **args)
{
  Function *f = NULL;
  CompileUnit *u = NULL;
  u32 pc = armNextPC;
  if(elfGetCurrentFunction(pc,
                           &f, &u)) {
    Object *o = f->parameters;
    while(o) {
      printf("%s=", o->name);
      debuggerPrintValue(f, o);
      o = o->next;
    }

    o = f->variables;
    while(o) {
      bool visible = o->startScope ? pc>=o->startScope : true;
      if(visible)
        visible = o->endScope ? pc < o->endScope : true;
      if(visible) {
        printf("%s=", o->name);
        debuggerPrintValue(f, o);
      }
      o = o->next;      
    }
  } else {
    printf("No information for current address\n");
  }  
}

void debuggerNext(int n, char **args)
{
  int count = 1;
  if(n == 2) {
    sscanf(args[1], "%d", &count);
  }
  for(int i = 0; i < count; i++) {
    if(debuggerAtBreakpoint) {
      debuggerContinueAfterBreakpoint();
      debuggerEnableBreakpoints(false);
    } else 
      emulator.emuMain(1);
  }
  debuggerDisableBreakpoints();
  Function *f = NULL;
  CompileUnit *u = NULL;
  u32 a = armNextPC;
  if(elfGetCurrentFunction(a, &f, &u)) {
    char *file;
    int line = elfFindLine(u, f, a, &file);
    
    printf("File %s, function %s, line %d\n", file, f->name,
           line);
  }
  debuggerRegisters(0, NULL);
}

void debuggerContinue(int n, char **args)
{
  if(debuggerAtBreakpoint)
    debuggerContinueAfterBreakpoint();
  debuggerEnableBreakpoints(false);
  debugger = false;
}

void debuggerSignal(int sig,int number)
{
  switch(sig) {
  case 4:
    {
      printf("Illegal instruction at %08x\n", armNextPC);
      debugger = true;
    }
    break;
  case 5:
    {
      armPrevPC = armPrevPC2; /*hack for bkpts*/
      debuggerDisableBreakpoints();

      if (number == 255) { /* run-to bkpt */
	debuggerBkptRunTo.address = 0;
      } else {
	printf("Breakpoint %d reached\n", number);
      }

      debugger = true;
      debuggerAtBreakpoint = true;
      debuggerBreakpointNumber = number;

      
      Function *f = NULL;
      CompileUnit *u = NULL;
      
      if(elfGetCurrentFunction(armNextPC, &f, &u)) {
        char *file;
        int line = elfFindLine(u,f,armNextPC,&file);
        printf("File %s, function %s, line %d\n", file, f->name,
               line);
      }
    }
    break;
  default:
    printf("Unknown signal %d\n", sig);
    break;
  }
}

void debuggerBreakList(int, char **)
{
  printf("Num Address  Type  Symbol\n");
  printf("--- -------- ----- ------\n");
  for(int i = 0; i < debuggerNumOfBreakpoints; i++) {
    printf("%3d %08x %s %s\n",i, debuggerBreakpointList[i].address,
           debuggerBreakpointList[i].size ? "ARM" : "THUMB",
           elfGetAddressSymbol(debuggerBreakpointList[i].address));
  }
}

void debuggerBreakDelete(int n, char **args)
{
  if(n == 2) {
    int n = 0;
    sscanf(args[1], "%d", &n);
    printf("Deleting breakpoint %d (%d)\n", n, debuggerNumOfBreakpoints);
    if(n >= 0 && n < debuggerNumOfBreakpoints) {
      n++;
      if(n < debuggerNumOfBreakpoints) {
        for(int i = n; i < debuggerNumOfBreakpoints; i++) {
          debuggerBreakpointList[i-1].address = 
            debuggerBreakpointList[i].address;
          debuggerBreakpointList[i-1].value = 
            debuggerBreakpointList[i].value;
          debuggerBreakpointList[i-1].size = 
            debuggerBreakpointList[i].size;
        }
      }
      debuggerNumOfBreakpoints--;
    }
  } else
    debuggerUsage("bd");    
}

void debuggerBreak(int n, char **args)
{
  if(n == 2) {
    u32 address = 0;
    u32 value = 0;
    int type = 0;
    char *s = args[1];
    char c = *s;
    if(strchr(s, ':')) {
      char *name = s;
      char *l = strchr(s, ':');
      *l++ = 0;
      int line = atoi(l);

      u32 addr;
      Function *f;
      CompileUnit *u;
      
      if(elfFindLineInModule(&addr, name, line)) {
        if(elfGetCurrentFunction(addr, &f, &u)) {
          u32 addr2;
          if(elfGetSymbolAddress(f->name, &addr2, &value, &type)) {
            address = addr;
          } else {
            printf("Unable to get function symbol data\n");
            return;
          }
        } else {
          printf("Unable to find function for address\n");
          return;
        }
      } else {
        printf("Unable to find module or line\n");
        return;
      }
    } else if(c >= '0' && c <= '9') {
      int line = atoi(s);
      Function *f;
      CompileUnit *u;
      u32 addr;
      
      if(elfGetCurrentFunction(armNextPC, &f, &u)) {
        if(elfFindLineInUnit(&addr, u, line)) {
          if(elfGetCurrentFunction(addr, &f, &u)) {
            u32 addr2;
            if(elfGetSymbolAddress(f->name, &addr2, &value, &type)) {
              address = addr;
            } else {
              printf("Unable to get function symbol data\n");
              return;
            }
          } else {
            printf("Unable to find function for address\n");
            return;
          }
        } else {
          printf("Unable to find line\n");
          return;
        }
      } else {
        printf("Cannot find current function\n");
        return;
      }
    } else {
      if(!elfGetSymbolAddress(s, &address, &value, &type)) {
        printf("Function %s not found\n", args[1]);
        return;
      }
    }
    if(type == 0x02 || type == 0x0d) {
      int i = debuggerNumOfBreakpoints;
      int size = 0;
      // new code -- handle eabi change
      if (address & 1) {
        type = 0x0d;
        address &= ~1;
      }
      // end new code 
      if(type == 2)
        size = 1;
      debuggerBreakpointList[i].address = address;
      debuggerBreakpointList[i].value = type == 0x02 ?
        debuggerReadMemory(address) : debuggerReadHalfWord(address);
      debuggerBreakpointList[i].size = size;
      //      debuggerApplyBreakpoint(address, i, size);
      debuggerNumOfBreakpoints++;
      if(size)
        printf("Added ARM breakpoint at %08x\n", address);        
      else
        printf("Added THUMB breakpoint at %08x\n", address);
    } else {
      printf("%s is not a function symbol\n", args[1]); 
    }
  } else
    debuggerUsage(args[0]);  
}

void debuggerRunTo(int n, char **args)
{
  if (n == 2){
    armState ? debuggerRunToArm(n, args) : debuggerRunToThumb(n, args);
    return;
  } else
    debuggerUsage(args[0]);
}

void debuggerRunToThumb(int n, char **args)
{
  if (n == 2) {
    u32 address = 0;
    if (!dexp_eval(args[1], &address)){
      printf("Invalid expression.\n");
      return;
    }

    debuggerBkptRunTo.address = address;
    debuggerBkptRunTo.value = debuggerReadHalfWord(address);
    debuggerBkptRunTo.size = 0;
    debuggerContinue(0, NULL);
  } else
    debuggerUsage(args[0]);
}

void debuggerRunToArm(int n, char **args)
{
  if (n == 2) {
    u32 address = 0;
    if (!dexp_eval(args[1], &address)){
      printf("Invalid expression.\n");
      return;
    }

    debuggerBkptRunTo.address = address;
    debuggerBkptRunTo.value = debuggerReadMemory(address);
    debuggerBkptRunTo.size = 1;
    debuggerContinue(0, NULL);
  } else
    debuggerUsage(args[0]);
}


void debuggerBreakThumb(int n, char **args)
{
  if(n == 2) {
    u32 address = 0;
    sscanf(args[1],"%x", &address);
    int i = debuggerNumOfBreakpoints;
    debuggerBreakpointList[i].address = address;
    debuggerBreakpointList[i].value = debuggerReadHalfWord(address);
    debuggerBreakpointList[i].size = 0;
    //    debuggerApplyBreakpoint(address, i, 0);
    debuggerNumOfBreakpoints++;
    printf("Added THUMB breakpoint at %08x\n", address);
  } else
    debuggerUsage("bt");    
}

void debuggerBreakArm(int n, char **args)
{
  if(n == 2) {
    u32 address = 0;
    sscanf(args[1],"%x", &address);
    int i = debuggerNumOfBreakpoints;
    debuggerBreakpointList[i].address = address;
    debuggerBreakpointList[i].value = debuggerReadMemory(address);
    debuggerBreakpointList[i].size = 1;
    //    debuggerApplyBreakpoint(address, i, 1);
    debuggerNumOfBreakpoints++;
    printf("Added ARM breakpoint at %08x\n", address);
  } else
    debuggerUsage("ba");
}

void debuggerBreakOnRead(u32 address, int size)
{

  if (debuggerInDB(armState ? reg[15].I - 4 : reg[15].I - 2))
    return;

  if(size == 2)
    printf("Breakpoint (on read) address %08x value:%08x\n",
           address, debuggerReadMemory(address));
  else if(size == 1)
    printf("Breakpoint (on read) address %08x value:%04x\n",
           address, debuggerReadHalfWord(address));
  else
    printf("Breakpoint (on read) address %08x value:%02x\n",
           address, debuggerReadByte(address));
  debugger = true;

}

void debuggerBreakOnWrite(u32 address, u32 value, int size)
{
  if (debuggerInDB(armState ? reg[15].I - 4 : reg[15].I - 2))
    return;

  if(size == 2)
    printf("Breakpoint (on write) address %08x old:%08x new:%08x\n", 
           address, debuggerReadMemory(address), value);
  else if(size == 1)
    printf("Breakpoint (on write) address %08x old:%04x new:%04x\n", 
           address, debuggerReadHalfWord(address),(u16)value);
  else
    printf("Breakpoint (on write) address %08x old:%02x new:%02x\n", 
           address, debuggerReadByte(address), (u8)value);
  debugger = true;
}

void debuggerBreakReadClear(int n, char **args)
{
  for(int i=0;i<16;i++) {
    memset(map[i].read, 0, map[i].size >> 3);
  }
  printf("Cleared all break on read\n");
}

void debuggerBreakRead(int n, char **args)
{
  if(n == 3) {
    if(cheatsNumber != 0) {
      printf("Cheats are enabled. Cannot continue.\n");
      return;
    }

    u32 address = 0;
    if (!dexp_eval(args[1], &address)){
      printf("Invalid expression in address.\n");
      return;
    }
    u32 n = 0;
    if (!dexp_eval(args[2], &n)){
      printf("Invalid expression in count.\n");
      return;
    }

    memoryMap *m = &map[address >> 24];
    if (m->read == NULL) {
      printf("Cannot place a breakpoint on this address.\n");
      return;
    }

    if ((address & m->mask) > m->size) {
      printf("Invalid address: %08x\n", address);
      return;
    }

    u32 final = address + n;
    if ((final & m->mask) > m->size) {
      printf("Invalid byte count: %08X\n", n);
      return;
    }

    printf("Added break on read at %08x for %d bytes\n", address, n);

    address = address & m->mask;
    for(u32 i = 0; i < n; i++) {
      BitSet(m->read, address + i);
    }
  } else
    debuggerUsage("bpr");
}


void debuggerBreakWriteClear(int n, char **args)
{
  for(int i=0;i<16;i++) {
    memset(map[i].write, 0, map[i].size >> 3);
  }
  printf("Cleared all break on write\n");
}

void debuggerBreakWrite(int n, char **args)
{
  if(n == 3) {
    if(cheatsNumber != 0) {
      printf("Cheats are enabled. Cannot continue.\n");
      return;
    }

    u32 address = 0;
    if (!dexp_eval(args[1], &address)){
      printf("Invalid expression in address.\n");
      return;
    }
    u32 n = 0;
    if (!dexp_eval(args[2], &n)){
      printf("Invalid expression in count.\n");
      return;
    }

    memoryMap *m = &map[address >> 24];
    if (m->write == NULL) {
      printf("Cannot place a breakpoint on this address.\n");
      return;
    }

    if ((address & m->mask) > m->size) {
      printf("Invalid address: %08x\n", address);
      return;
    }

    u32 final = address + n;
    if ((final & m->mask) > m->size) {
      printf("Invalid byte count: %08X\n", n);
      return;
    }

    printf("Added break on write at %08x for %d bytes\n", address, n);

    address = address & m->mask;
    for(u32 i = 0; i < n; i++) {
      BitSet(m->write, address + i);
    }
  } else
    debuggerUsage("bpw");    
}

void debuggerDisassembleDo(int (*func)(u32, char*, int), u32 start, int count)
{
  char buffer[80];
  int i = 0;
  int len = 0;
  char format[30];
  u32 pc = start;
  u32 finish;

  for(i = 0; i < count; i++) {
    int l = strlen(elfGetAddressSymbol(pc+4*i));
    if(l > len)
      len = l;
  }
  sprintf(format, "%%08x %%-%ds %%s\n", len);
  
  
  if (func == &disThumb)
    finish = start + (count << 1);
  else
    finish = start + (count << 2);
  
  /* note: this no longer outputs <count> instructions (a 4 byte bl was 
     previously counted as a single instruction), instead it now just
     disassembles over the range start - finish. This was changed so that
     disassembleNear produces the correct output when a bl instruction
     appeared before the current one */
     
  while(pc < finish) {
    u32 addr = pc;
    pc += func(pc, buffer, 2);
    printf(format, addr, elfGetAddressSymbol(addr), buffer);
  }

}

void debuggerDisassembleArm(int n, char **args)
{

  u32 pc = reg[15].I;
  pc -= 4;
  u32 count = 20;
  if(n >= 2) {
    if (!dexp_eval(args[1], &pc)){
      printf("Invalid expression in address.\n");
      return;
    }
  }
  if(pc & 3) {
    printf("Misaligned address %08x\n", pc);
    pc &= 0xfffffffc;
  }
  if(n >= 3) {
    if (!dexp_eval(args[2], &count)){
      printf("Invalid expression in count.\n");
      return;
    }
  }

  debuggerDisassembleDo(&disArm, pc, count);

}

void debuggerDisassembleThumb(int n, char **args)
{
  u32 pc = reg[15].I;
  pc -= 2;
  u32 count = 20;
  if(n >= 2) {
    if (!dexp_eval(args[1], &pc)){
      printf("Invalid expression in address.\n");
      return;
    }
  }
  if(pc & 1) {
    printf("Misaligned address %08x\n", pc);
    pc &= 0xfffffffe;
  }
  if(n >= 3) {
    if (!dexp_eval(args[2], &count)){
      printf("Invalid expression in count.\n");
      return;
    }
  }

  debuggerDisassembleDo(&disThumb, pc, count);
}

void debuggerDisassemble(int n, char **args)
{
  if(armState)
    debuggerDisassembleArm(n, args);
  else
    debuggerDisassembleThumb(n, args);
}

void debuggerDisassembleNearDo(int count)
{
  if(armState) {
    debuggerDisassembleDo(&disArm, reg[15].I - 4 - (count << 2), count);
    printf("> ");
    debuggerDisassembleDo(&disArm, reg[15].I - 4, count+1);
  } else {
    debuggerDisassembleDo(&disThumb, reg[15].I - 2 - (count << 1), count);
    printf("> ");
    debuggerDisassembleDo(&disThumb, reg[15].I - 2, count+1);
  }

}

void debuggerDisassembleNear(int n, char **args)
{
    int count = 10;

    if (n >= 2) {
      sscanf(args[1], "%d", &count);
    }

    debuggerDisassembleNearDo(count);
    
 
}

void debuggerContinueAfterBreakpoint()
{
  printf("Continuing after breakpoint\n");
  debuggerEnableBreakpoints(true);
  emulator.emuMain(1);
  debuggerAtBreakpoint = false;
}

void debuggerRegisters(int, char **)
{

  printf("R00=%08x R04=%08x R08=%08x R12=%08x\n",
         reg[0].I, reg[4].I, reg[8].I, reg[12].I);
  printf("R01=%08x R05=%08x R09=%08x R13=%08x\n",
         reg[1].I, reg[5].I, reg[9].I, reg[13].I);
  printf("R02=%08x R06=%08x R10=%08x R14=%08x\n",
         reg[2].I, reg[6].I, reg[10].I, reg[14].I);
  printf("R03=%08x R07=%08x R11=%08x R15=%08x\n",
         reg[3].I, reg[7].I, reg[11].I, reg[15].I);
  printf("CPSR=%08x (%c%c%c%c%c%c%c Mode: %02x)\n",
         reg[16].I,
         (N_FLAG ? 'N' : '.'),
         (Z_FLAG ? 'Z' : '.'),
         (C_FLAG ? 'C' : '.'),
         (V_FLAG ? 'V' : '.'),
         (armIrqEnable ? '.' : 'I'),
         ((!(reg[16].I & 0x40)) ? '.' : 'F'),
         (armState ? '.' : 'T'),
         armMode);

  if(armState) {
    debuggerDisassembleDo(&disArm, armPrevPC, 1);
    printf("> ");
    debuggerDisassembleDo(&disArm, reg[15].I - 4, 2);
  } else {
    debuggerDisassembleDo(&disThumb, armPrevPC, 1);
    printf("> ");
    debuggerDisassembleDo(&disThumb, reg[15].I - 2, 2);
  }

}

void debuggerIoVideo()
{
  printf("DISPCNT  = %04x\n", DISPCNT);
  printf("DISPSTAT = %04x\n", DISPSTAT);
  printf("VCOUNT   = %04x\n", VCOUNT);
  printf("BG0CNT   = %04x\n", BG0CNT);
  printf("BG1CNT   = %04x\n", BG1CNT);
  printf("BG2CNT   = %04x\n", BG2CNT);
  printf("BG3CNT   = %04x\n", BG3CNT);
  printf("WIN0H    = %04x\n", WIN0H);
  printf("WIN0V    = %04x\n", WIN0V);
  printf("WIN1H    = %04x\n", WIN1H);
  printf("WIN1V    = %04x\n", WIN1V);
  printf("WININ    = %04x\n", WININ);
  printf("WINOUT   = %04x\n", WINOUT);
  printf("MOSAIC   = %04x\n", MOSAIC);
  printf("BLDMOD   = %04x\n", BLDMOD);
  printf("COLEV    = %04x\n", COLEV);
  printf("COLY     = %04x\n", COLY);
}

void debuggerIoVideo2()
{
  printf("BG0HOFS  = %04x\n", BG0HOFS);
  printf("BG0VOFS  = %04x\n", BG0VOFS);
  printf("BG1HOFS  = %04x\n", BG1HOFS);
  printf("BG1VOFS  = %04x\n", BG1VOFS);
  printf("BG2HOFS  = %04x\n", BG2HOFS);
  printf("BG2VOFS  = %04x\n", BG2VOFS);
  printf("BG3HOFS  = %04x\n", BG3HOFS);
  printf("BG3VOFS  = %04x\n", BG3VOFS);
  printf("BG2PA    = %04x\n", BG2PA);
  printf("BG2PB    = %04x\n", BG2PB);
  printf("BG2PC    = %04x\n", BG2PC);
  printf("BG2PD    = %04x\n", BG2PD);
  printf("BG2X     = %08x\n", (BG2X_H<<16)|BG2X_L);
  printf("BG2Y     = %08x\n", (BG2Y_H<<16)|BG2Y_L);
  printf("BG3PA    = %04x\n", BG3PA);
  printf("BG3PB    = %04x\n", BG3PB);
  printf("BG3PC    = %04x\n", BG3PC);
  printf("BG3PD    = %04x\n", BG3PD);
  printf("BG3X     = %08x\n", (BG3X_H<<16)|BG3X_L);
  printf("BG3Y     = %08x\n", (BG3Y_H<<16)|BG3Y_L);
}

void debuggerIoDMA()
{
  printf("DM0SAD   = %08x\n", (DM0SAD_H<<16)|DM0SAD_L);
  printf("DM0DAD   = %08x\n", (DM0DAD_H<<16)|DM0DAD_L);
  printf("DM0CNT   = %08x\n", (DM0CNT_H<<16)|DM0CNT_L);  
  printf("DM1SAD   = %08x\n", (DM1SAD_H<<16)|DM1SAD_L);
  printf("DM1DAD   = %08x\n", (DM1DAD_H<<16)|DM1DAD_L);
  printf("DM1CNT   = %08x\n", (DM1CNT_H<<16)|DM1CNT_L);  
  printf("DM2SAD   = %08x\n", (DM2SAD_H<<16)|DM2SAD_L);
  printf("DM2DAD   = %08x\n", (DM2DAD_H<<16)|DM2DAD_L);
  printf("DM2CNT   = %08x\n", (DM2CNT_H<<16)|DM2CNT_L);  
  printf("DM3SAD   = %08x\n", (DM3SAD_H<<16)|DM3SAD_L);
  printf("DM3DAD   = %08x\n", (DM3DAD_H<<16)|DM3DAD_L);
  printf("DM3CNT   = %08x\n", (DM3CNT_H<<16)|DM3CNT_L);    
}

void debuggerIoTimer()
{
  printf("TM0D     = %04x\n", TM0D);
  printf("TM0CNT   = %04x\n", TM0CNT);
  printf("TM1D     = %04x\n", TM1D);
  printf("TM1CNT   = %04x\n", TM1CNT);
  printf("TM2D     = %04x\n", TM2D);
  printf("TM2CNT   = %04x\n", TM2CNT);
  printf("TM3D     = %04x\n", TM3D);
  printf("TM3CNT   = %04x\n", TM3CNT);
}

void debuggerIoMisc()
{
  printf("P1       = %04x\n", P1);  
  printf("IE       = %04x\n", IE);
  printf("IF       = %04x\n", IF);
  printf("IME      = %04x\n", IME);
}

void debuggerIo(int n, char **args)
{
  if(n == 1) {
    debuggerIoVideo();
    return;
  }
  if(!strcmp(args[1], "video"))
    debuggerIoVideo();
  else if(!strcmp(args[1], "video2"))
    debuggerIoVideo2();
  else if(!strcmp(args[1], "dma"))
    debuggerIoDMA();
  else if(!strcmp(args[1], "timer"))
    debuggerIoTimer();
  else if(!strcmp(args[1], "misc"))
    debuggerIoMisc();
  else printf("Unrecognized option %s\n", args[1]);
}

void debuggerEditByte(int n, char **args)
{
  if(n == 3) {
    u32 address;
    u32 value;
    if (!dexp_eval(args[1], &address)) {
      printf("Invalid expression in address.\n");
      return;
    }
    if (!dexp_eval(args[2], &value)) {
      printf("Invalid expression in value.\n");
    }
    debuggerWriteByte(address, (u8)value);
  } else
    debuggerUsage("eb");    
}

void debuggerEditHalfWord(int n, char **args)
{
  if(n == 3) {
    u32 address;
    u32 value;
    if (!dexp_eval(args[1], &address)) {
      printf("Invalid expression in address.\n");
      return;
    }
    if(address & 1) {
      printf("Error: address must be half-word aligned\n");
      return;
    }
    if (!dexp_eval(args[2], &value)) {
      printf("Invalid expression in value.\n");
    }
    debuggerWriteHalfWord(address, (u16)value);
  } else
    debuggerUsage("eh");        
}

void debuggerEdit(int n, char **args)
{
  if(n == 3) {
    u32 address;
    u32 value;
    if (!dexp_eval(args[1], &address)) {
      printf("Invalid expression in address.\n");
      return;
    }
    if(address & 3) {
      printf("Error: address must be word aligned\n");
      return;
    }
    if (!dexp_eval(args[2], &value)) {
      printf("Invalid expression in value.\n");
    }
    debuggerWriteMemory(address, (u32)value);
  } else
    debuggerUsage("ew");    
}


#define ASCII(c) (c) < 32 ? '.' : (c) > 127 ? '.' : (c)

void debuggerMemoryByte(int n, char **args)
{
  if(n == 2) {
    u32 addr = 0;

    if (!dexp_eval(args[1], &addr)){
      printf("Invalid expression\n");
      return;
    }

    for(int loop = 0; loop < 16; loop++) {
      int a = debuggerReadByte(addr);
      int b = debuggerReadByte(addr+1);
      int c = debuggerReadByte(addr+2);
      int d = debuggerReadByte(addr+3);
      int e = debuggerReadByte(addr+4);
      int f = debuggerReadByte(addr+5);
      int g = debuggerReadByte(addr+6);
      int h = debuggerReadByte(addr+7);
      int i = debuggerReadByte(addr+8);
      int j = debuggerReadByte(addr+9);
      int k = debuggerReadByte(addr+10);
      int l = debuggerReadByte(addr+11);
      int m = debuggerReadByte(addr+12);
      int n = debuggerReadByte(addr+13);
      int o = debuggerReadByte(addr+14);
      int p = debuggerReadByte(addr+15);
      
      printf("%08x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c\n",
             addr,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,
             ASCII(a),ASCII(b),ASCII(c),ASCII(d),
             ASCII(e),ASCII(f),ASCII(g),ASCII(h),
             ASCII(i),ASCII(j),ASCII(k),ASCII(l),
             ASCII(m),ASCII(n),ASCII(o),ASCII(p));
      addr += 16;
    }
  } else
    debuggerUsage("mb");    
}

void debuggerMemoryHalfWord(int n, char **args)
{
  if(n == 2) {
    u32 addr = 0;

    if (!dexp_eval(args[1], &addr)){
      printf("Invalid expression\n");
      return;
    }

    addr = addr & 0xfffffffe;

    for(int loop = 0; loop < 16; loop++) {
      int a = debuggerReadByte(addr);
      int b = debuggerReadByte(addr+1);
      int c = debuggerReadByte(addr+2);
      int d = debuggerReadByte(addr+3);
      int e = debuggerReadByte(addr+4);
      int f = debuggerReadByte(addr+5);
      int g = debuggerReadByte(addr+6);
      int h = debuggerReadByte(addr+7);
      int i = debuggerReadByte(addr+8);
      int j = debuggerReadByte(addr+9);
      int k = debuggerReadByte(addr+10);
      int l = debuggerReadByte(addr+11);
      int m = debuggerReadByte(addr+12);
      int n = debuggerReadByte(addr+13);
      int o = debuggerReadByte(addr+14);
      int p = debuggerReadByte(addr+15);
      
      printf("%08x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c\n",
             addr,b,a,d,c,f,e,h,g,j,i,l,k,n,m,p,o,
             ASCII(a),ASCII(b),ASCII(c),ASCII(d),
             ASCII(e),ASCII(f),ASCII(g),ASCII(h),
             ASCII(i),ASCII(j),ASCII(k),ASCII(l),
             ASCII(m),ASCII(n),ASCII(o),ASCII(p));
      addr += 16;
    }
  } else
    debuggerUsage("mh");    
}

void debuggerMemory(int n, char **args)
{
  if(n == 2) {
    u32 addr = 0;
    if (!dexp_eval(args[1], &addr)){
      printf("Invalid expression\n");
      return;
    }
    addr = addr & 0xfffffffc;
    for(int loop = 0; loop < 16; loop++) {
      int a = debuggerReadByte(addr);
      int b = debuggerReadByte(addr+1);
      int c = debuggerReadByte(addr+2);
      int d = debuggerReadByte(addr+3);

      int e = debuggerReadByte(addr+4);
      int f = debuggerReadByte(addr+5);
      int g = debuggerReadByte(addr+6);
      int h = debuggerReadByte(addr+7);

      int i = debuggerReadByte(addr+8);
      int j = debuggerReadByte(addr+9);
      int k = debuggerReadByte(addr+10);
      int l = debuggerReadByte(addr+11);

      int m = debuggerReadByte(addr+12);
      int n = debuggerReadByte(addr+13);
      int o = debuggerReadByte(addr+14);
      int p = debuggerReadByte(addr+15);
      
      printf("%08x %02x%02x%02x%02x %02x%02x%02x%02x %02x%02x%02x%02x %02x%02x%02x%02x %c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c\n",
             addr,d,c,b,a,h,g,f,e,l,k,j,i,p,o,n,m,
             ASCII(a),ASCII(b),ASCII(c),ASCII(d),
             ASCII(e),ASCII(f),ASCII(g),ASCII(h),
             ASCII(i),ASCII(j),ASCII(k),ASCII(l),
             ASCII(m),ASCII(n),ASCII(o),ASCII(p));
      addr += 16;
    }
  } else
    debuggerUsage("mw");    
}

void debuggerQuit(int, char **)
{
  char buffer[10];
  printf("Are you sure you want to quit (y/n)? ");
  fgets(buffer, 1024, stdin);
  
  if(buffer[0] == 'y' || buffer[0] == 'Y') {
    debugger = false;
    emulating = false;
  }
}

void debuggerOutput(char *s, u32 addr)
{
  if(s)
    printf(s);
  else {
    char c;

    c = debuggerReadByte(addr);
    addr++;
    while(c) {
      putchar(c);
      c = debuggerReadByte(addr);
      addr++;
    }
  }
}

#ifdef HAVE_LIBREADLINE
// Read a string, and return a pointer to it.  Returns NULL on EOF.
char *
rl_gets ()
{
  // If the buffer has already been allocated, return the memory
  // to the free pool. 
  if (line_read)
  {
    free (line_read);
    line_read = (char *)NULL;
  }

  // Get a line from the user.
  line_read = readline ("debugger> ");

  // If the line has any text in it, save it on the history.
  if (line_read && *line_read)
    add_history (line_read);
  else {
    // if nothing, repeat last command (gdb style)
    HIST_ENTRY * prev = previous_history();
    if (prev)
      return prev->line;
  }

  return (line_read);
}
#endif


char* strqtok (char* string, const char* ctrl)
{
  static char* nexttoken = NULL;
  char* str;

  if (string != NULL)
    str = string;
  else {
    if (nexttoken == NULL)
      return NULL;
    str = nexttoken;
  };

  char deli [32];
  memset (deli, 0, 32 * sizeof (char));
  while (*ctrl)
  {
    deli [*ctrl >> 3] |= (1 << (*ctrl & 7));
    ctrl++;
  };
  // can't allow to be set
  deli ['"' >> 3] &= ~(1 << ('"' & 7));

  // jump over leading delimiters
  while ((deli [*str >> 3] & (1 << (*str & 7))) && *str)
    str++;

  if (*str == '"')
  {
    string = ++str;

    // only break if another quote or end of string is found
    while ((*str != '"') && *str)
      str++;
  } else {
    string = str;

    // break on delimiter
    while (!(deli [*str >> 3] & (1 << (*str & 7))) && *str)
      str++;
  };

  if (string == str)
  {
    nexttoken = NULL;
    return NULL;
  } else {
    if (*str)
    {
      *str = 0;
      nexttoken = str + 1;
    } else
      nexttoken = NULL;

    return string;
  };
};

void debuggerMain()
{
  char buffer[1024];
  char *commands[10];
  int commandCount = 0;
  
  if(emulator.emuUpdateCPSR)
    emulator.emuUpdateCPSR();
  debuggerRegisters(0, NULL);
  
  while(debugger) {
    systemSoundPause();
    commandCount = 0;
#ifdef HAVE_LIBREADLINE
    char *s = rl_gets();
#else
    printf("debugger> ");
    char *s = fgets(buffer, 1024, stdin);
#endif

    commands[0] = strqtok(s, " \t\n");
    if(commands[0] == NULL)
      continue;
    commandCount++;
    while((s = strqtok(NULL, " \t\n"))) {
      commands[commandCount++] = s;
      if(commandCount == 10)
        break;
    }

    for(int j = 0; ; j++) {
      if(debuggerCommands[j].name == NULL) {
        printf("Unrecognized command %s. Type h for help.\n", commands[0]);
        break;
      }
      if(!strcmp(commands[0], debuggerCommands[j].name)) {
        debuggerCommands[j].function(commandCount, commands);
        break;
      }
    } 
  }
}

void debuggerWriteState(int n, char **args)
{
  int num;

  sscanf(args[1],"%d",&num);

  if (num > 0 && num < 11)
    sdlWriteState(num-1);
  else
    debuggerUsage("save");
}

void debuggerReadState(int n, char **args)
{
  int num;

  sscanf(args[1],"%d",&num);

  if (num > 0 && num < 11)
    sdlReadState(num-1);
  else
    debuggerUsage("load");
}

void debuggerDumpLoad(int n, char** args)
{
  u32 address;
  char *file;
  FILE *f;
  int c;

  if (n==3){
    file=args[1];

    if (!dexp_eval(args[2], &address)){
      printf("Invalid expression in address.\n");
      return;
    }

    f=fopen(file,"rb");
    if (f==NULL){
      printf("Error opening file.\n");
      return;
    }

    fseek(f,0,SEEK_END);
    int size=ftell(f);
    fseek(f,0,SEEK_SET);

    for (int i=0;i<size;i++) {
      c = fgetc(f);
      if(c == -1)
	break;
      debuggerWriteByte(address,c);
      address++;
    }

    fclose(f);
  } else
    debuggerUsage("dload");
}

void debuggerDumpSave(int n, char** args)
{
  u32 address;
  u32 size;
  char *file;
  FILE *f;
	
  if (n==4){
    file=args[1];
    if (!dexp_eval(args[2], &address)){
      printf("Invalid expression in address.\n");
      return;
    }
    if (!dexp_eval(args[2], &size)){
      printf("Invalid expression in size");
      return;
    }

    f=fopen(file,"wb");
    if(f==NULL){
      printf("Error opening file.\n");
      return;
    }

    for(u32 i = 0; i < size; i++) {
      fputc(debuggerReadByte(address), f);
      address++;
    }

    fclose(f);
  } else
    debuggerUsage("dsave");
}

void debuggerEditRegister(int n, char **args)
{
  if(n == 3) {
    int r;
    u32 val;
    if (toupper(args[1][0]) == 'R') {
      sscanf((char *)(args[1] + 1), "%d", &r);
    } else {
      sscanf(args[1], "%d", &r);
    }
    if(r > 16) {
      printf("Error: Register must be valid (0-16)\n");
      return;
    }
    if (!dexp_eval(args[2], &val)) {
      printf("Invalid expression in value.\n");
      return;
    }
    reg[r].I=val;
    printf("R%02d=%08X\n", r, val);
  } else
    debuggerUsage("er");    
}

void debuggerFindText(int n, char **args)
{
  if ((n == 4) || (n == 3))
  {
    SearchResults = 0;
    sscanf(args[1], "%x", &SearchStart);
  
    if (n == 4)
	{
      sscanf(args[2], "%u", &SearchMaxMatches);
      strncpy((char*) SearchData, args[3], 64);
	  SearchLength = strlen(args[3]);
	}
	else if (n == 3)
	{
      strncpy((char*) SearchData, args[2], 64);
	  SearchLength = strlen(args[2]);
	};

	if (SearchLength > 64)
	{
	  printf ("Entered string (length: %d) is longer than 64 bytes and was cut.\n", SearchLength);
	  SearchLength = 64;
	};

	debuggerDoSearch ();

  } else
    debuggerUsage("ft");
};

void debuggerFindHex(int n, char **args)
{
  if ((n == 4) || (n == 3))
  {
    SearchResults = 0;
    sscanf(args[1], "%x", &SearchStart);

	char SearchHex [128];
    if (n == 4)
	{
      sscanf(args[2], "%u", &SearchMaxMatches);
      strncpy(SearchHex, args[3], 128);
	  SearchLength = strlen(args[3]);
	}
	else if (n == 3)
	{
      strncpy(SearchHex, args[2], 128);
	  SearchLength = strlen(args[2]);
	};

	if (SearchLength & 1)
	  printf ("Unaligned bytecount: %d,5. Last digit (%c) cut.\n", SearchLength / 2, SearchHex [SearchLength - 1]);

	SearchLength /= 2;

	if (SearchLength > 64)
	{
	  printf ("Entered string (length: %d) is longer than 64 bytes and was cut.\n", SearchLength);
	  SearchLength = 64;
	};

	for (unsigned int i = 0; i < SearchLength; i++)
	{
	  unsigned int cbuf = 0;
	  sscanf (&SearchHex [i << 1], "%02x", &cbuf);
      SearchData [i] = cbuf;
	};

	debuggerDoSearch ();

  } else
    debuggerUsage("fh");
};

void debuggerFindResume(int n, char **args)
{
  if ((n == 1) || (n == 2))
  {
    if (SearchLength == 0)
	{
	  printf("Error: No search in progress. Start a search with ft or fh.\n");
      debuggerUsage("fr");
	  return;
	};

	if (n == 2)
	  sscanf(args[1], "%u", &SearchMaxMatches);

	debuggerDoSearch();

  } else
    debuggerUsage("fr");
};

void debuggerDoSearch()
{
  int count = 0;

  while (true)
  {
    unsigned int final = SearchStart + SearchLength - 1;
    u8* end;
    u8* start;

    switch (SearchStart >> 24)
    {
    case 0:
      if (final > 0x00003FFF) { SearchStart = 0x02000000; continue; }
        else { start = bios + (SearchStart & 0x3FFF); end = bios + 0x3FFF; break; };
    case 2:
      if (final > 0x0203FFFF) { SearchStart = 0x03000000; continue; }
        else { start = workRAM + (SearchStart & 0x3FFFF); end = workRAM + 0x3FFFF; break; };
    case 3:
      if (final > 0x03007FFF) { SearchStart = 0x04000000; continue; }
        else { start = internalRAM + (SearchStart & 0x7FFF); end = internalRAM + 0x7FFF; break; };
    case 4:
      if (final > 0x040003FF) { SearchStart = 0x05000000; continue; }
        else { start = ioMem + (SearchStart & 0x3FF); end = ioMem + 0x3FF; break; };
    case 5:
      if (final > 0x050003FF) { SearchStart = 0x06000000; continue; }
        else { start = paletteRAM + (SearchStart & 0x3FF); end = paletteRAM + 0x3FF; break; };
    case 6:
      if (final > 0x0601FFFF) { SearchStart = 0x07000000; continue; }
        else { start = vram + (SearchStart & 0x1FFFF); end = vram + 0x1FFFF; break; };
    case 7:
      if (final > 0x070003FF) { SearchStart = 0x08000000; continue; }
        else { start = oam + (SearchStart & 0x3FF); end = oam + 0x3FF; break; };
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
	  if (final <= 0x09FFFFFF)
		{ start = rom + (SearchStart & 0x01FFFFFF); end = rom + 0x01FFFFFF; break; };
	default:
      printf ("Search completed.\n");
      SearchLength = 0;
      return;
    };

    end -= SearchLength - 1;
    u8 firstbyte = SearchData [0];
    while (start <= end)
    {
      while ((start <= end) && (*start != firstbyte))
        start++;

      if (start > end)
        break;

      unsigned int p = 1;
      while ((start [p] == SearchData [p]) && (p < SearchLength))
        p++;

      if (p == SearchLength)
      {
        printf ("Search result (%d): %08x\n", count + SearchResults, AddressToGBA (start));
        count++;
        if (count == SearchMaxMatches)
        {
          SearchStart = AddressToGBA (start + p);
		  SearchResults += count;
          return;
        };

        start += p;		// assume areas don't overlap; alternative: start++;
      } else
        start++;
    };

    SearchStart = AddressToGBA (end + SearchLength - 1) + 1;
  };
};

unsigned int AddressToGBA(u8* mem)
{
  if(mem >= &bios[0] && mem <= &bios[0x3fff])
    return 0x00000000 + (mem - &bios[0]);
  else if(mem >= &workRAM[0] && mem <= &workRAM[0x3ffff])
    return 0x02000000 + (mem - &workRAM[0]);
  else if(mem >= &internalRAM[0] && mem <= &internalRAM[0x7fff])
    return 0x03000000 + (mem - &internalRAM[0]);
  else if(mem >= &ioMem[0] && mem <= &ioMem[0x3ff])
    return 0x04000000 + (mem - &ioMem[0]);
  else if(mem >= &paletteRAM[0] && mem <= &paletteRAM[0x3ff])
    return 0x05000000 + (mem - &paletteRAM[0]);
  else if(mem >= &vram[0] && mem <= &vram[0x1ffff])
    return 0x06000000 + (mem - &vram[0]);
  else if(mem >= &oam[0] && mem <= &oam[0x3ff])
    return 0x07000000 + (mem - &oam[0]);
  else if(mem >= &rom[0] && mem <= &rom[0x1ffffff])
    return 0x08000000 + (mem - &rom[0]);
  else
    return 0xFFFFFFFF;
};
 


void debuggerDontBreak(int n, char **args)
{
  if(n == 2) {
    u32 address = 0;
    sscanf(args[1],"%x", &address);
    int i = debuggerNumOfDontBreak;
    if (i > NUMBEROFDB)
    {
      printf("Can't have this many DB entries");
      return;
    }
    debuggerNoBreakpointList[i] = address;
    debuggerNumOfDontBreak++;
    printf("Added Don't Break at %08x\n", address);
  } else
    debuggerUsage("db");
}

void debuggerDontBreakClear(int n, char **args)
{
  if(n == 1) {
    debuggerNumOfDontBreak = 0;
    printf("Cleared Don't Break list.\n");
  } else
    debuggerUsage("dbc");
}

// checks that the given address is in the DB list
bool debuggerInDB(u32 address)
{

  for (int i = 0; i < debuggerNumOfDontBreak ; i++)
  {
	  if (debuggerNoBreakpointList[i] == address)
	      return true;
  }

  return false;

}


void debuggerEval(int n, char **args)
{
  if(n == 2) {
    u32 result = 0;
    if (dexp_eval(args[1], &result)) {
      printf(" =$%08X\n", result);
    } else {
      printf("Invalid expression\n");
    }
  } else
    debuggerUsage("eval");
}

void debuggerVar(int n, char **args)
{
  u32 val;

  if (n < 2) {
    dexp_listVars();
    return;
  }

  if (strcmp(args[1], "set") == 0) {
    
    if (n < 4) {
      printf("No expression specified.\n");
      return;
    }

    if (!dexp_eval(args[3], &val)){
      printf("Invalid expression.\n");
      return;
    }

    dexp_setVar(args[2], val);
    printf("%s = $%08x\n", args[2], val);
    return;
  }

  if (strcmp(args[1], "list") == 0) {
    dexp_listVars();
    return;
  }

  if (strcmp(args[1], "save") == 0) {
    if (n < 3) {
      printf("No file specified.\n");
      return;
    }
    dexp_saveVars(args[2]);
    return;
  }

  if (strcmp(args[1], "load") == 0) {
    if (n < 3){
      printf("No file specified.\n");
      return;
    }
    dexp_loadVars(args[2]);
    return;
  }

  printf("Unrecognized sub-command.\n");
    
}

  
void debuggerTrace(int n, char **args)
{
  if (n < 2) {
    debuggerUsage(args[0]);
    return;
  }

  if (strcmp(args[1], "start") == 0) {
    if (strlen(debuggerTraceFilename) == 0) {
      strcpy(debuggerTraceFilename, "trace.log");
    }

    debuggerTraceFile = fopen(debuggerTraceFilename, "w");
    if (debuggerTraceFile) {
      debuggerTracing = true;
      debuggerTraceSkipped = false;
      for (int i=0; i<256; i++) {
	if (map[i].trace) {
	  memset(map[i].trace, 0, map[i].size >> 3);
	}
      }
    } else {
      printf("Could not open file for writing: %s", debuggerTraceFilename);
    }
    
    return;
  }

  if (strcmp(args[1], "stop") == 0) {
    debuggerTracing = false;
    if (debuggerTraceFile) {
      fclose(debuggerTraceFile);
    }
    return;
  }

  if (strcmp(args[1], "file") == 0) {
    if (n < 3){
      printf("Currently set to '%s'\n", debuggerTraceFilename);
      return;
    }

    strcpy(debuggerTraceFilename, args[2]);
    return;
  }

  if (strcmp(args[1], "reg") == 0) {
    if (n < 3) {
      printf("Currently %sdisplaying registers in trace.\n", debuggerTraceReg ? "":"not ");
      return;
    }

    if (strcmp(args[2], "on") == 0)
      debuggerTraceReg = true;
    else if (strcmp(args[2], "off") == 0)
      debuggerTraceReg = false;
    else
      printf("Specify 'on' or 'off'\n");

    return;
  }

  if (strcmp(args[1], "once") == 0) {
    if (n < 3) {
      printf("Trace once currently %sactivated.\n", debuggerTraceOnce ? "":"not ");
      return;
    }

    if (strcmp(args[2], "on") == 0)
      debuggerTraceOnce = true;
    else if (strcmp(args[2], "off") == 0)
      debuggerTraceOnce = false;
    else
      printf("Specify 'on' or 'off'\n");

    return;
  }

  printf("Unrecognised command '%s'", args[1]);
      
}

void debuggerTraceDo()
{
  char buffer[800];
  char dis[200];

  memoryMap *m = &map[armNextPC >> 24];

  if (debuggerTraceOnce) {
    if(m->trace){
      if (BitGet(m->trace, armNextPC & m->mask)) {
	if (!debuggerTraceSkipped) {
	  fprintf(debuggerTraceFile, "\n");
	  debuggerTraceSkipped = true;
	}
	debuggerTraceLast = armNextPC;
	return;
      }
      BitSet(m->trace, armNextPC & m->mask);
      if (debuggerTraceSkipped) {
	debuggerTraceSkipped = false;
	fprintf(debuggerTraceFile, "[last: %08X]\n", debuggerTraceLast);
      }
    }
  }

  if (armState) {
    disArm(armNextPC, dis, 2);
  } else {
    disThumb(armNextPC, dis, 2);
  }

  if (debuggerTraceReg) {
    sprintf(buffer, "%08X %-40s R00=%08x R01=%08x R02=%08x R03=%08x R04=%08x R05=%08x R06=%08x R07=%08x R08=%08x R09=%08x R10=%08x R11=%08x R12=%08x R13=%08x R14=%08x R15=%08x\n",
	  armNextPC, dis,reg[0].I, reg[1].I, reg[2].I, reg[3].I, reg[4].I, reg[5].I,
	    reg[6].I, reg[7].I, reg[8].I, reg[9].I, reg[10].I, reg[11].I,
	    reg[12].I, reg[13].I, reg[14].I, reg[15].I);

    fprintf(debuggerTraceFile, buffer);
  } else {
    fprintf(debuggerTraceFile, "%08X %s\n", armNextPC, dis);
  }

  debuggerTraceLast = armNextPC;
}
