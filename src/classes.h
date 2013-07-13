/***************************************************************************
* Base class for all runtime classes
*
* Copyright (c) 2013 Randy Hollines
* All rights reserved.
*
* Redistribution and uses in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*
* - Redistributions of source code must retain the above copyright
* notice, this list of conditions and the following disclaimer.
* - Redistributions in binary form must reproduce the above copyright
* notice, this list of conditions and the following disclaimer in
* the documentation and/or other materials provided with the distribution.
* - Neither the name of the Substance team nor the names of its
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

#ifndef __CLASS_H__
#define __CLASS_H__

#include "common.h"
#include "jit/jit_common.h"

using namespace std;

typedef void(*Operation)(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
typedef void(*Method)(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count);

/****************************
* Base class for built-in types
****************************/
class RuntimeClass {
  unordered_map<wstring, Operation> operations;
  unordered_map<wstring, Method> methods;
  
protected:
  RuntimeClass() {
  }

  virtual ~RuntimeClass() {
  }

  void AddOperation(const wstring &name, Operation oper) {
    operations.insert(pair<wstring, Operation>(name, oper));
  }
  
  void AddMethod(const wstring &name, Method method) {
    methods.insert(pair<wstring, Method>(name, method));
  }

public:
  Operation GetOperation(const wstring name) {
    unordered_map<wstring, Operation>::iterator result = operations.find(name);
    if(result != operations.end()) {
      return result->second;
    }

    return NULL;
  }

  Method GetMethod(const wstring name) {
    unordered_map<wstring, Method>::iterator result = methods.find(name);
    if(result != methods.end()) {
      return result->second;
    }
    
    return NULL;
  }
};

/****************************
* Boolean class
****************************/
class BooleanClass : public RuntimeClass {
  static BooleanClass* instance;

  BooleanClass() {
    // note that '&' and '|' are implemented as conditional jumps
    AddOperation(L"==", Equal);
    AddOperation(L"!=", NotEqual);    
  }

  ~BooleanClass() {
  }

public:
  static BooleanClass* Instance() {
    if(!instance) {
      instance = new BooleanClass;
    }

    return instance;
  }

  static void Equal(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void NotEqual(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
};

/****************************
* Integer class
****************************/
class IntegerClass : public RuntimeClass {
  static IntegerClass* instance;

  IntegerClass() {
    // operations
    AddOperation(L"+", Add);
    AddOperation(L"-", Subtract);
    AddOperation(L"*", Multiply);
    AddOperation(L"/", Divide);
    AddOperation(L"==", Equal);
    AddOperation(L"!=", NotEqual);
    AddOperation(L"<", Less);
    AddOperation(L">", Greater);
    AddOperation(L"<=", LessEqual);
    AddOperation(L">=", GreaterEqual);
    AddOperation(L"%", Modulo);
    // methods
    AddMethod(L"Abs", Abs);
    AddMethod(L"ToFloat", ToFloat);
  }

  ~IntegerClass() {
  }

public:
  static IntegerClass* Instance() {
    if(!instance) {
      instance = new IntegerClass;
    }

    return instance;
  }

  // operations
  static void Add(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Subtract(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Multiply(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Divide(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Modulo(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Equal(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void NotEqual(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Less(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Greater(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void LessEqual(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void GreaterEqual(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  
  // methods
  static void Abs(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count);
  static void ToFloat(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count);
};

/****************************
* Float class
****************************/
class FloatClass : public RuntimeClass {
  static FloatClass* instance;

  FloatClass() {
    AddOperation(L"+", Add);
    AddOperation(L"-", Subtract);
    AddOperation(L"*", Multiply);
    AddOperation(L"/", Divide);
    AddOperation(L"==", Equal);
    AddOperation(L"!=", NotEqual);
    AddOperation(L"<", Less);
    AddOperation(L">", Greater);
    AddOperation(L"<=", LessEqual);
    AddOperation(L">=", GreaterEqual);
    AddOperation(L"%", Modulo);
    // methods
    AddMethod(L"ToInteger", ToInteger);
  }
  
  ~FloatClass() {
  }

public:
  static FloatClass* Instance() {
    if(!instance) {
      instance = new FloatClass;
    }

    return instance;
  }

  static void Add(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Subtract(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Multiply(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Divide(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Modulo(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Equal(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void NotEqual(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Less(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Greater(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void LessEqual(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void GreaterEqual(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  // methods
  static void ToInteger(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count);
};

class Classes {
  static Classes* instance;
  unordered_map<wstring, RuntimeClass*> classes;

 public:
  static Classes* Instance() {
    if(!instance) {
      instance = new Classes;
    }
    
    return instance;
  }

  Classes() {
    classes[L"Integer"] = IntegerClass::Instance();
  }

  ~Classes() {
  }
  
  RuntimeClass* GetClass(const wstring &name) {
    unordered_map<wstring, RuntimeClass*>::iterator result = classes.find(name);
    if(result != classes.end()) {
      return result->second;
    }

    return NULL;
  }
};
#endif
