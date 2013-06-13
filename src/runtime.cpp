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
#include "jit/jit_common.h"
#include "jit/ia32/jit_ia32.h"

using namespace runtime;

#define HIT_THRESHOLD 3

// delegates operation to the appropriate type class
#define CALC(name, left, right, jit_instrs, is_recording) {   \
  left = PopValue();                                          \
  right = PopValue();                                         \
  if(left->klass) {                                           \
    Operation oper = left->klass->GetOperation(name);         \
    (*oper)(left, right, left, jit_instrs, is_recording);     \
    PushValue(left);                                          \
  }                                                           \
  else {                                                      \
    wcerr << L">>> invalid operation <<<" << endl;            \
    exit(1);                                                  \
  }                                                           \
}

/****************************
 * TODO: doc
 ****************************/
void Runtime::Run()
{
#ifdef _DEBUG
  wcout << L"---------- Executing Code ---------" << endl;
#endif
	
  // runtime variables
	Value* frame = new Value[8];
  Value *left, *right;
  
  // find 'hotspots' for JIT compiling
  bool is_loop = false;
  bool is_recording = false;
  vector<jit::JitInstruction*> jit_instrs;

  // execute code
	size_t ip = 0;  
  Instruction* instruction = instructions[ip++];
  while(instruction->type != RTRN) {   
    switch(instruction->type) {
    case LOAD_INT_LIT:
      left = new Value; // GetPoolValue();
      left->type = INT_VALUE;
      left->klass = IntegerClass::Instance();
      left->value.int_value = instruction->operand1;
#ifdef _DEBUG
			wcout << L"LOAD_INT_LIT: value=" << left->value.int_value << endl;
#endif
      PushValue(left);
      // record JIT instructions
      if(is_recording) {
        jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_LIT, left->value.int_value));
      }
      break;
      
    case LOAD_FLOAT_LIT:
      left = new Value; // GetPoolValue();
      left->type = FLOAT_VALUE;
      left->klass = FloatClass::Instance();
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
      memcpy(left, &frame[instruction->operand1], sizeof(Value));
			PushValue(left);
      // record JIT instructions
      if(is_recording) {
        switch(right->type) {
        case INT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_VAR, instruction->operand1, jit::LOCL));
          break;

        case FLOAT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_FLOAT_VAR, instruction->operand1, jit::LOCL));
          break;

        default:
          break;
        }
      }
      break;
      
    case STOR_VAR:
#ifdef _DEBUG
			wcout << L"STOR_VAR: id=" << instruction->operand1 << endl;
#endif
      left = PopValue();
      memcpy(&frame[instruction->operand1], left, sizeof(Value));
      // record JIT instructions
      if(is_recording) {
        switch(right->type) {
        case INT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::STOR_INT_VAR, instruction->operand1, jit::LOCL));
          break;

        case FLOAT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::STOR_FLOAT_VAR, instruction->operand1, jit::LOCL));
          break;

        default:
          break;
        }
      }
      break;

		case LBL:
#ifdef _DEBUG
			wcout << L"LBL: id=" << instruction->operand1 << L", hit_count=" << instruction->operand2 << endl;
#endif
      instruction->operand2++;
      if(instruction->operand2 > HIT_THRESHOLD) {
        is_recording = true;
      }
      is_loop = false;
			break;
			
		case JMP:
			switch(instruction->operand1) {
				// unconditional jump
			case 0:
#ifdef _DEBUG
				wcout << L"JMP: unconditional" << endl;
#endif/*
				/
				if(is_recording) {
          // compile into native code and execute
          jit::JitCompiler compiler(jit_instrs);
          jit::jit_fun_ptr jit_fun = compiler.Compile();
          (*jit_fun)(frame, NULL, NULL);
          // clean up
          is_recording = false;
          jit_instrs.clear();
          // take jump
          ip = jump_table[instruction->operand2];
        }
        else {
          // look for loop and take jump
          size_t next_ip = jump_table[instruction->operand2];
          is_loop = next_ip < ip;
				  ip = next_ip;
        }
			*/
				ip = jump_table[instruction->operand2];
				break;
				
				// jump true
			case 1:
#ifdef _DEBUG
				wcout << L"JMP: true" << endl;
#endif
				left = PopValue();
				if(left->type != BOOL_VALUE) {
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
				if(left->type != BOOL_VALUE) {
					cerr << L">>> Expected a boolean value <<<" << endl;
					exit(1);
				}
				if(!left->value.int_value) {
					ip = jump_table[instruction->operand2];
				}
        // record JIT instructions
        if(is_recording) {
          jit_instrs.push_back(new jit::JitInstruction(jit::GUARD, (long)jump_table[instruction->operand2]));
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
      CALC(L"<", left, right, jit_instrs, is_recording);
      break;
      
    case GTR_EQL:
      break;

    case LES_EQL:
      break;
			
    case ADD:
#ifdef _DEBUG
			wcout << L"ADD" << endl;
#endif
      CALC(L"+", left, right, jit_instrs, is_recording);
      break;

    case SUB:
      break;

    case MUL:
#ifdef _DEBUG
			wcout << L"MUL" << endl;
#endif
      CALC(L"*", left, right, jit_instrs, is_recording);
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
      }
      break;
    }
    // update
    instruction = instructions[ip++];
  } 

  delete[] frame;
  frame = NULL;

#ifdef _DEBUG
	wcout << L"--------------------------" << endl;
	wcout << L"ending stack pos=" << execution_stack_pos << endl;
#endif
}
