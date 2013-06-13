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

/****************************
 * Base class for built-in types
 ****************************/
class RuntimeClass {
  unordered_map<wstring, Operation> operations;

 protected:
  RuntimeClass() {
  }
  
  virtual ~RuntimeClass() {
  }

  void AddOperation(const wstring &name, Operation oper) {
    operations.insert(pair<wstring, Operation>(name, oper));
  }

public:
  Operation GetOperation(const wstring name) {
    unordered_map<wstring, Operation>::iterator result = operations.find(name);
    if(result != operations.end()) {
      return result->second;
    }

    return NULL;
  }
};

/****************************
 * Integer class
 ****************************/
class IntegerClass : public RuntimeClass {
  static IntegerClass* instance;

  IntegerClass();
  ~IntegerClass();

 public:
  static IntegerClass* Instance() {
    if(!instance) {
      instance = new IntegerClass;
    }

    return instance;
  }

  static void Add(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Multiply(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Less(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
};

/****************************
 * Float class
 ****************************/
class FloatClass : public RuntimeClass {
  static FloatClass* instance;

  FloatClass();
  ~FloatClass();

 public:
  static FloatClass* Instance() {
    if(!instance) {
      instance = new FloatClass;
    }

    return instance;
  }

  static void Add(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Multiply(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
  static void Less(Value &left, Value &right, Value &result, vector<jit::JitInstruction*> &jit_instrs, bool is_recording);
};

#endif
