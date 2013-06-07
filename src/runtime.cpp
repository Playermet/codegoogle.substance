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
#ifdef _DEBUG
  wcerr << L"---------- Executing Code ---------" << endl;
#endif
	
	Value* frame[8];
  for(int i = 0; i < 8; i++) {
    frame[i] = new Value;
  }

	size_t ip = 0;
  Value *left, *right;
  Instruction* instruction = instructions[ip++];
  while(instruction->type != RTRN) {   
    switch(instruction->type) {
    case LOAD_INT_LIT:
      left = GetPoolValue();
      left->type = INT_VALUE;
      left->value.int_value = instruction->operand1;
#ifdef _DEBUG
			wcout << L"LOAD_INT_LIT" << endl;
#endif
      PushValue(left);
      break;
      
    case LOAD_FLOAT_LIT:
      left = GetPoolValue();
      left->type = FLOAT_VALUE;
      left->value.float_value = instruction->operand3;
#ifdef _DEBUG
			wcout << L"LOAD_FLOAT_LIT: " << endl;
#endif
      PushValue(left);
      break;
      
    case LOAD_VAR:
#ifdef _DEBUG
			wcout << L"LOAD_VAR: id=" << instruction->operand1 << endl;
#endif
      left = GetPoolValue();
      memcpy(left, frame[instruction->operand1], sizeof(Value));
			PushValue(left);
      break;
      
    case STOR_VAR:
#ifdef _DEBUG
			wcout << L"STOR_VAR: id=" << instruction->operand1 << endl;
#endif
      left = PopValue();
      memcpy(frame[instruction->operand1], left, sizeof(Value));
      break;

		case LBL:
#ifdef _DEBUG
			wcout << L"LBL" << endl;
#endif
			break;
			
		case JMP:
			switch(instruction->operand1) {
				// unconditional jump
			case 0:
#ifdef _DEBUG
				wcout << L"JMP: unconditional" << endl;
#endif
				ip = instruction->operand2;
				break;
				
				// jump true
			case 1:
#ifdef _DEBUG
				wcout << L"JMP: true" << endl;
#endif
				left = PopValue();
				if(left->type != BOOL_TYPE) {
					cerr << L">>> Expected a boolean value <<<" << endl;
					exit(1);
				}
				if(left->value.int_value) {
					ip = jump_table[instruction->operand2];
				}
				break;
				
				// jump false
			case -1:
#ifdef _DEBUG
				wcout << L"JMP: false" << endl;
#endif
				left = PopValue();
				if(left->type != BOOL_TYPE) {
					cerr << L">>> Expected a boolean value <<<" << endl;
					exit(1);
				}
				if(!left->value.int_value) {
					ip = jump_table[instruction->operand2];
				}
				break;
			}
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
#ifdef _DEBUG
			wcout << L"LES" << endl;
#endif
			ExecuteLess();
      break;
      
    case GTR_EQL:
      break;

    case LES_EQL:
      break;
			
    case ADD:
#ifdef _DEBUG
			wcout << L"ADD" << endl;
#endif
      ExecuteAdd();
      break;

    case SUB:
      break;

    case MUL:
#ifdef _DEBUG
			wcout << L"MUL" << endl;
#endif
      ExecuteMultiply();
      break;

    case DIV:
      break;

    case MOD:
      break;

    case RTRN:
      break;

    case DUMP_VALUE:
#ifdef _DEBUG
			wcout << L"DUMP" << endl;
#endif
      left = PopValue();
      switch(left->type) {
      case INT_VALUE:
        wcout << L"type=integer, value=" << left->value.int_value << endl;
        break;
    
      case FLOAT_VALUE:
        wcout << L"type=float, value=" << left->value.float_value << endl;
        break;
    
      case STRING_VALUE:
        wcout << L"type=string, value=" << (wchar_t*)left->value.pointer_value << endl;
        break;
      }
      break;
    }
    // update
    instruction = instructions[ip++];
  } 

#ifdef _DEBUG
	wcout << L"--------------------------" << endl;
	wcout << L"ending stack pos=" << execution_stack.size() << endl;
	wcout << L"pool size=" << value_pool.size() << endl;
#endif
}

void Runtime::ExecuteAdd()
{
  Value* left = PopValue();
  Value* right = PopValue();
	
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
  Value* left = PopValue();
  Value* right = PopValue();
	
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

void Runtime::ExecuteLess()
{
  Value* left = PopValue();
  Value* right = PopValue();
  
  switch(left->type) {
    // left
  case INT_VALUE:
    switch(right->type) {
      // right
    case INT_VALUE:
      left->type = BOOL_TYPE;
      left->value.int_value = left->value.int_value < right->value.int_value;
      break;
      
    case FLOAT_VALUE:
      left->type = BOOL_TYPE;
      left->value.float_value = left->value.int_value < right->value.float_value;
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
      left->type = BOOL_TYPE;
       left->value.float_value = left->value.float_value < right->value.int_value;
      break;

    case FLOAT_VALUE:
      left->type = BOOL_TYPE;
      left->value.float_value = left->value.float_value < right->value.float_value;
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
