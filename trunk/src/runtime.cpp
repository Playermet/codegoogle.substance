/***************************************************************************
 * Runtime system
 *
 * Copyright (c) 2013 Randy Hollines
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met
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

#include "runtime.h"

/****************************
 * TODO: doc
 ****************************/
void Runtime::Run()
{
  map<const wstring, Value*> local_table;
  Instruction* instruction;
  Value *left, *right;
 
#ifdef _DEBUG
  wcerr << L"---------- Executing ---------" << endl;
#endif
  
  size_t ip = 0;
  do {
    instruction = instructions[ip++];
    switch(instruction->type) {
    case LOAD_INT_LIT:
      left = GetPoolValue();
      left->type = INT_VALUE;
      left->value.int_value = instruction->operand1;
      PushValue(left);
      break;
      
    case LOAD_FLOAT_LIT:
      left = GetPoolValue();
      left->type = FLOAT_VALUE;
      left->value.float_value = instruction->operand3;
      PushValue(left);
      break;
      
    case LOAD_VAR: {
      map<const wstring, Value*>::iterator result = local_table.find(instruction->operand4);
      if(result != local_table.end()) {
        PushValue(result->second);
      }
      else {
        wcout << L">>> undefined variable: " << instruction->operand4 << endl;
        exit(1);
      }
    }
      break;
      
    case STOR_VAR:
      left = PopValue();
      local_table.insert(pair<const wstring, Value*>(instruction->operand4, left));
      break;

    case AND:
      break;

    case OR:
      break;

    case EQL:
      break;

    case NEQL:
      break;

    case GTR:
      break;

    case LES:
      break;
      
    case GTR_EQL:
      break;

    case LES_EQL:
      break;
    case ADD:
      ExecuteAdd();
      break;

    case SUB:
      break;

    case MUL:
      ExecuteMultiply();
      break;

    case DIV:
      break;

    case MOD:
      break;

    case RTRN:
      break;

    case DUMP_VALUE:
      left = PopValue();
      switch(left->type) {
      case INT_VALUE:
        wcout << left->value.int_value << endl;
        break;
    
      case FLOAT_VALUE:
        wcout << left->value.float_value << endl;
        break;
    
      case STRING_VALUE:
        wcout << (wchar_t*)left->value.pointer_value << endl;
        break;
      }
      break;
    }
  }
  while(instruction->type != RTRN);
}

void Runtime::ExecuteAdd()
{
  Value* right = PopValue();
  Value* left = PopValue();
  
  switch(left->type) {
    // left
  case INT_VALUE:
    switch(right->type) {
      // right
    case INT_VALUE:
      left->type = INT_VALUE;
      left->value.int_value += right->value.int_value;
      break;
      
    case FLOAT_VALUE:
      left->type = FLOAT_VALUE;
      left->value.float_value = left->value.int_value + right->value.float_value;
      break;
      
    case STRING_VALUE:
      // TODO: implement
      break;

    default:
      wcerr << ">>> invalid operation <<<" << endl;
      break;
    }
    ReleasePoolValue(right);
    break;
    
  case FLOAT_VALUE:
    // right
    switch(right->type) {
    case INT_VALUE:
      left->type = FLOAT_VALUE;
      left->value.float_value = left->value.float_value + right->value.int_value;
      break;

    case FLOAT_VALUE:
      left->type = FLOAT_VALUE;
      left->value.float_value += right->value.float_value;
      break;

    case STRING_VALUE:
      // TODO: implement
      break;

    default:
      wcerr << ">>> invalid operation <<<" << endl;
      break;
    }
    ReleasePoolValue(right);
    break;

  case STRING_VALUE:
    // right
    switch(right->type) {
    case INT_VALUE:
      // TODO: implement
      break;

    case FLOAT_VALUE:
      // TODO: implement
      break;

    case STRING_VALUE:
      // TODO: implement
      break;

    default:
      wcerr << ">>> invalid operation <<<" << endl;
      break;
    }
    ReleasePoolValue(right);
    break;
    
  default:
    wcerr << ">>> invalid operation <<<" << endl;
    break;
  }
  
  PushValue(left);
}

void Runtime::ExecuteMultiply()
{
  Value* right = PopValue();
  Value* left = PopValue();
  
  switch(left->type) {
    // left
  case INT_VALUE:
    switch(right->type) {
      // right
    case INT_VALUE:
      left->type = INT_VALUE;
      left->value.int_value *= right->value.int_value;
      break;
      
    case FLOAT_VALUE:
      left->type = FLOAT_VALUE;
      left->value.float_value = left->value.int_value * right->value.float_value;
      break;
      
    case STRING_VALUE:
      // TODO: implement
      break;

    default:
      wcerr << ">>> invalid operation <<<" << endl;
      break;
    }
    ReleasePoolValue(right);
    break;
    
  case FLOAT_VALUE:
    // right
    switch(right->type) {
    case INT_VALUE:
      left->type = FLOAT_VALUE;
      left->value.float_value = left->value.float_value * right->value.int_value;
      break;

    case FLOAT_VALUE:
      left->type = FLOAT_VALUE;
      left->value.float_value *= right->value.float_value;
      break;

    case STRING_VALUE:
      // TODO: implement
      break;

    default:
      wcerr << ">>> invalid operation <<<" << endl;
      break;
    }
    ReleasePoolValue(right);
    break;

  case STRING_VALUE:
    // right
    switch(right->type) {
    case INT_VALUE:
      // TODO: implement
      break;

    case FLOAT_VALUE:
      // TODO: implement
      break;

    case STRING_VALUE:
      // TODO: implement
      break;

    default:
      wcerr << ">>> invalid operation <<<" << endl;
      break;
    }
    ReleasePoolValue(right);
    break;
    
  default:
    wcerr << ">>> invalid operation <<<" << endl;
    break;
  }

  PushValue(left);
}
