/***************************************************************************
* Language model
*
* Copyright (c) 2013, Randy Hollines
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*
* - Redistributions of source code must retain the above copyright
* notice, this list of conditions and the following disclaimer.
* - Redistributions in binary form must reproduce the above copyright
* notice, this list of conditions and the following disclaimer in
* the documentation and/or other materials provided with the distribution.
* - Neither the name of the Objeck Team nor the names of its
* contributors may be used to endorse or promote products derived
* from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
*  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
***************************************************************************/

#ifndef __COMMON_H__
#define __COMMON_H__

#include <algorithm>
#include <iostream>
#include <sstream>
#include <fstream>
#include <stack>
#include <vector>
#include <list>
#include <set>
#include <map>
#include <string>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#include <process.h>
#include <unordered_map>
using namespace stdext;
#else
#include <unordered_map>
#include <pthread.h>
#include <stdint.h>
#endif

using namespace std;

class RuntimeClass;
struct _Value;

// prototype for JIT function
namespace jit {
  typedef long (*jit_fun_ptr)(struct _Value* frame, void* inst_mem, void* cls_mem);
}

// basic datatypes
#define INT_T long
#define FLOAT_T double
#define CHAR_T wchar_t
#define BYTE_T char

// jump operands
#define JMP_TRUE 1
#define JMP_FALSE 0
#define JMP_UNCND -1

/****************************
* Runtime instructions
****************************/
enum InstructionType {
  // literals
  LOAD_TRUE_LIT = -256,
  LOAD_FALSE_LIT,
  LOAD_INT_LIT,
  LOAD_FLOAT_LIT,
  // variables
  LOAD_VAR,
  STOR_VAR,
  LOAD_ARY_VAR,
  STOR_ARY_VAR,
  // logical  
  EQL,
  NEQL,
  GTR,
  LES,
  GTR_EQL,
  LES_EQL,  
  // mathematical
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  // bitwise
  BIT_AND,
  BIT_OR,
  // jumps
  JMP,
  LBL,
  // functions and traps
  CALL_CLS_FUNC,
  CALL_FUNC,
  RTRN,
  // misc
  DUMP_VALUE
};

class Instruction {
public:
  Instruction() {
    native_code = NULL;
  }

  ~Instruction() {
  }

  InstructionType type;
  INT_T operand1;
  INT_T operand2;
  INT_T operand3;
  FLOAT_T operand4;
  wstring operand5;
  wstring operand6;
  jit::jit_fun_ptr native_code;
};

/****************************
* Runtime types and values
****************************/
enum RuntimeType {
  BOOL_VALUE = -5,
  INT_VALUE,
  FLOAT_VALUE,
  ARY_VALUE,
  CLS_VALUE,
  UNINIT_VALUE
};

/****************************
* 'Abstract' value type
****************************/
typedef struct _Value { 
  RuntimeType type;
  RuntimeClass* klass;
  
  union _value {
    BYTE_T byte_value;
    CHAR_T char_value;
    INT_T int_value;
    FLOAT_T float_value;
    void* ptr_value;
  } value;
} Value;

/****************************
* Holder for runtime program
****************************/
class ExecutableFunction {
  wstring name;
  int local_count;
  int parameter_count;
  vector<Instruction*>* block_instructions; 
  unordered_map<long, size_t>* jump_table;
  set<size_t> leaders;
  bool returns_value;

public:
  ExecutableFunction(const wstring &name, int local_count, int parameter_count, vector<Instruction*>* block_instructions, 
                     unordered_map<long, size_t>* jump_table, set<size_t> &leaders, bool returns_value) {
      this->name = name;
      this->parameter_count = parameter_count;
      this->local_count = local_count;
      this->block_instructions = block_instructions;
      this->jump_table = jump_table;
      this->leaders = leaders;
      this->returns_value = returns_value;
  }

  ~ExecutableFunction() {
    if(jump_table) {
      delete jump_table;
      jump_table = NULL;
    }

    if(block_instructions) {
      delete block_instructions;
      block_instructions = NULL;
    }
  }

  const wstring GetName() {
    return name;
  }

  int GetParameterCount() {
    return parameter_count;
  }

  int GetLocalCount() {
    return local_count;
  }

  bool ReturnsValue() {
    return returns_value;
  }

  vector<Instruction*>* GetInstructions() {
    return block_instructions; 
  }

  unordered_map<long, size_t>* GetJumpTable() {
    return jump_table;
  }
};

/****************************
* Local symbol table
****************************/
class InnerTable {
  unordered_map<wstring, int> table;

public:
  InnerTable() {
  }

  ~InnerTable() {		
  }

  void AddEntry(const wstring &name, int id) {
    table.insert(pair<wstring, int>(name, id));
  }

  int GetEntry(const wstring &name) {
    unordered_map<wstring, int>::iterator result = table.find(name);
    if(result != table.end()) {
      return result->second;
    }

    return -1;
  }

  bool HasEntry(const wstring &name) {
    return GetEntry(name)  > -1;
  }
};

/****************************
* Hierarchical symbol table
****************************/
class SymbolTable {
  deque<InnerTable*> table_hierarchy;
  vector<InnerTable*> all_tables;
  INT_T entry_id;

public:
  SymbolTable() {
    entry_id = 1;
  }

  ~SymbolTable() {
    while(!all_tables.empty()) {
      InnerTable* tmp = all_tables.front();
      all_tables.erase(all_tables.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }
  }

  void NewScope() {
    InnerTable* table = new InnerTable;
    table_hierarchy.push_front(table);
    all_tables.push_back(table);
  }

  bool PreviousScope() {
    if(!table_hierarchy.empty()) {
      table_hierarchy.pop_front();
      return true;
    }

    return false;
  }

  bool AddEntry(const wstring &name) {
    if(!table_hierarchy.empty()) {
      table_hierarchy.front()->AddEntry(name, entry_id++);
      return true;
    }

    return false;
  }

  int GetEntry(const wstring &name) {
    for(size_t i = 0; i < table_hierarchy.size(); ++i) {
      int value = table_hierarchy[i]->GetEntry(name);
      if(value > -1) {
        return value;
      }
    }

    return -1;
  }

  bool HasEntry(const wstring &name) {
    return GetEntry(name) > -1;
  }

  int GetEntryCount() {
    return entry_id;
  }
};	

/****************************
* Holder for runtime program
****************************/
class ExecutableProgram {
  ExecutableFunction* global_function;
  unordered_map<wstring, ExecutableFunction*> functions;

public:
  ExecutableProgram() {
  }

  ~ExecutableProgram() {
    if(global_function) {
      delete global_function;
      global_function = NULL;
    }

    unordered_map<wstring, ExecutableFunction*>::iterator iter;
    for(iter = functions.begin(); iter != functions.end(); ++iter) {
      ExecutableFunction* tmp = iter->second;
      delete tmp;
      tmp = NULL;
    }
    functions.clear();
  }

  void AddFunction(ExecutableFunction* function) {
    functions.insert(pair<wstring, ExecutableFunction*>(function->GetName(), function));
  }

  ExecutableFunction* GetFunction(const wstring &name) {
    unordered_map<wstring, ExecutableFunction*>::iterator result = functions.find(name);
    if(result != functions.end()) {
      return result->second;
    }

    return NULL;
  }

  void SetGlobal(ExecutableFunction* global_function) {
    this->global_function = global_function;
  }

  ExecutableFunction* GetGlobal() {
    return global_function;
  }
};

/****************************
* Utility functions
****************************/
inline wstring IntToString(int v) {
  wostringstream str;
  str << v;
  return str.str();
}

/****************************
* Converts a UTF-8 bytes to
* native a unicode string
****************************/
static bool BytesToUnicode(const std::string &in, std::wstring &out) {    
#ifdef _WIN32
  // allocate space
  int wsize = MultiByteToWideChar(CP_UTF8, 0, in.c_str(), -1, NULL, 0);
  if(!wsize) {
    return false;
  }
  wchar_t* buffer = new wchar_t[wsize];

  // convert
  int check = MultiByteToWideChar(CP_UTF8, 0, in.c_str(), -1, buffer, wsize);
  if(!check) {
    delete[] buffer;
    buffer = NULL;
    return false;
  }

  // create string
  out.append(buffer, wsize - 1);

  // clean up
  delete[] buffer;
  buffer = NULL;  
#else
  // allocate space
  size_t size = mbstowcs(NULL, in.c_str(), in.size());
  if(size == (size_t)-1) {
    return false;
  }
  wchar_t* buffer = new wchar_t[size + 1];

  // convert
  size_t check = mbstowcs(buffer, in.c_str(), in.size());
  if(check == (size_t)-1) {
    delete[] buffer;
    buffer = NULL;
    return false;
  }
  buffer[size] = L'\0';

  // create string
  out.append(buffer, size);

  // clean up
  delete[] buffer;
  buffer = NULL;
#endif

  return true;
}

static std::wstring BytesToUnicode(const std::string &in) {
  std::wstring out;
  if(BytesToUnicode(in, out)) {
    return out;
  }

  return L"";
}

/****************************
* Converts UTF-8 bytes to
* native a unicode character
****************************/
static bool BytesToCharacter(const std::string &in, wchar_t &out) {
  std::wstring buffer;
  if(!BytesToUnicode(in, buffer)) {
    return false;
  }

  if(buffer.size() != 1) {
    return false;
  }

  out = buffer[0];  
  return true;
}

/****************************
* Converts a native string
* to UTF-8 bytes
****************************/
static bool UnicodeToBytes(const std::wstring &in, std::string &out) {
#ifdef _WIN32
  // allocate space
  int size = WideCharToMultiByte(CP_UTF8, 0, in.c_str(), -1, NULL, 0, NULL, NULL);
  if(!size) {
    return false;
  }
  char* buffer = new char[size];

  // convert std::string
  int check = WideCharToMultiByte(CP_UTF8, 0, in.c_str(), -1, buffer, size, NULL, NULL);
  if(!check) {
    delete[] buffer;
    buffer = NULL;
    return false;
  }

  // append output
  out.append(buffer, size - 1);

  // clean up
  delete[] buffer;
  buffer = NULL;
#else
  // convert std::string
  size_t size = wcstombs(NULL, in.c_str(), in.size());
  if(size == (size_t)-1) {
    return false;
  }
  char* buffer = new char[size + 1];

  wcstombs(buffer, in.c_str(), size);
  if(size == (size_t)-1) {
    delete[] buffer;
    buffer = NULL;
    return false;
  }
  buffer[size] = '\0';

  // create std::string      
  out.append(buffer, size);

  // clean up
  delete[] buffer;
  buffer = NULL;
#endif

  return true;
}

static std::string UnicodeToBytes(const std::wstring &in) {
  std::string out;
  if(UnicodeToBytes(in, out)) {
    return out;
  }

  return "";
}

/****************************
* Converts a native character
* to UTF-8 bytes
****************************/
static bool CharacterToBytes(wchar_t in, std::string &out) {
  if(in == L'\0') {
    return true;
  }

  wchar_t buffer[2];
  buffer[0] = in;
  buffer[1] = L'\0';

  if(!UnicodeToBytes(buffer, out)) {
    return false;
  }

  return true;
}

/****************************
* Loads a UTF-8 file into memory 
* and converts content into native
* Unicode format
****************************/
static wchar_t* LoadFileBuffer(const wstring &name, size_t& buffer_size) {
  char* buffer;
  string open_name(name.begin(), name.end());

  ifstream in(open_name.c_str(), ios_base::in | ios_base::binary | ios_base::ate);
  if(in.good()) {
    // get file size
    in.seekg(0, ios::end);
    buffer_size = (size_t)in.tellg();
    in.seekg(0, ios::beg);
    buffer = (char*)calloc(buffer_size + 1, sizeof(char));
    in.read(buffer, buffer_size);
    // close file
    in.close();
  }
  else {
    wcerr << L"Unable to open source file: " << name << endl;
    exit(1);
  }

  // convert unicode
#ifdef _WIN32
  int wsize = MultiByteToWideChar(CP_UTF8, 0, buffer, -1, NULL, 0);
  if(!wsize) {
    wcerr << L"Unable to open source file: " << name << endl;
    exit(1);
  }
  wchar_t* wbuffer = new wchar_t[wsize];
  int check = MultiByteToWideChar(CP_UTF8, 0, buffer, -1, wbuffer, wsize);
  if(!check) {
    wcerr << L"Unable to open source file: " << name << endl;
    exit(1);
  }
#else
  size_t wsize = mbstowcs(NULL, buffer, buffer_size);
  if(wsize == (size_t)-1) {
    delete buffer;
    wcerr << L"Unable to open source file: " << name << endl;
    exit(1);
  }
  wchar_t* wbuffer = new wchar_t[wsize + 1];
  size_t check = mbstowcs(wbuffer, buffer, buffer_size);
  if(check == (size_t)-1) {
    delete buffer;
    delete wbuffer;
    wcerr << L"Unable to open source file: " << name << endl;
    exit(1);
  }
  wbuffer[wsize] = L'\0';
#endif

  free(buffer);
  return wbuffer;
}


#endif
