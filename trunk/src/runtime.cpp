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

// Windows x64
#if defined(_WIN32) && defined(_X64)
#include "jit/amd64/win/jit_amd_llp64.h"
// Posix x64
#elif _X64
#include "jit/amd64/posix/jit_amd_lp64.h"
#else
// Win32 and Posix
#include "jit/ia32/jit_ia32.h"
#endif

using namespace runtime;

#define HIT_THRESHOLD 3

// delegates operation to the appropriate type class
#define CALC(name, left, right, jit_instrs, is_recording) { \
  left = PopValue();                                        \
  right = PopValue();                                       \
  if(left.klass) {																					\
  Operation oper = left.klass->GetOperation(name);          \
  (*oper)(left, right, left, jit_instrs, is_recording);     \
  PushValue(left);                                          \
  }                                                         \
  else {                                                    \
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
  wcout << L"========== Executing Code =========" << endl;
#endif

  // runtime variables
  Value* frame = new Value[8];
  Value left, right;

	// tracing JIT variables
	const INT_T base_jit_label = last_label_id;
	stack<Instruction*> jit_jump_labels;
	
  // execute code
  size_t ip = 0;  
  Instruction* instruction = instructions[ip++];
  while(instruction->type != RTRN) {   
    switch(instruction->type) {
    case RTRN:
      break;

    case LOAD_TRUE_LIT:
      left.type = BOOL_VALUE;
      left.klass = BooleanClass::Instance();
      left.value.int_value = 1;
#ifdef _DEBUG
      wcout << L"LOAD_TRUE_LIT: value=true" << endl;
#endif
      PushValue(left);
      // record JIT instructions
      if(is_recording) {
        jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_LIT, (long)1));
      }
      break;
      
    case LOAD_FALSE_LIT:
      left.type = BOOL_VALUE;
      left.klass = BooleanClass::Instance();
      left.value.int_value = 0;
#ifdef _DEBUG
      wcout << L"LOAD_FALSE_LIT: value=false" << endl;
#endif
      PushValue(left);
      // record JIT instructions
      if(is_recording) {
        jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_LIT, (long)0));
      }
      break;

    case LOAD_INT_LIT:
      left.type = INT_VALUE;
      left.klass = IntegerClass::Instance();
      left.value.int_value = instruction->operand1;
#ifdef _DEBUG
      wcout << L"LOAD_INT_LIT: value=" << left.value.int_value << endl;
#endif
      PushValue(left);
      // record JIT instructions
      if(is_recording) {
        jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_LIT, left.value.int_value));
      }
      break;

    case LOAD_FLOAT_LIT:
      left.type = FLOAT_VALUE;
      left.klass = FloatClass::Instance();
      left.value.float_value = instruction->operand3;
#ifdef _DEBUG
      wcout << L"LOAD_FLOAT_LIT: value=" << left.value.float_value << endl;
#endif
      PushValue(left);
      // record JIT instructions
      if(is_recording) {
        jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_FLOAT_LIT, left.value.float_value));
      }
      break;

    case LOAD_VAR:
#ifdef _DEBUG
      wcout << L"LOAD_VAR: id=" << instruction->operand1 << endl;
#endif
      left = frame[instruction->operand1];
      PushValue(left);
      // record JIT instructions
      if(is_recording) {
        switch(left.type) {
        case BOOL_VALUE:
        case INT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_VAR, instruction->operand1, jit::LOCL));
          break;

        case FLOAT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_FLOAT_VAR, instruction->operand1, jit::LOCL));
          break;

        default:
          wcerr << L"Invalid operation" << endl;
          exit(1);
        }
      }
      break;

    case STOR_VAR:
#ifdef _DEBUG
      wcout << L"STOR_VAR: id=" << instruction->operand1 << endl;
#endif
      left = PopValue();
      // record JIT instructions
      if(is_recording) {
        // ensure the type hasn't changed
        if(frame[instruction->operand1].type == left.type) {
          switch(right.type) {
          case BOOL_VALUE:
          case INT_VALUE:
            jit_instrs.push_back(new jit::JitInstruction(jit::STOR_INT_VAR, instruction->operand1, jit::LOCL));
            break;

          case FLOAT_VALUE:
            jit_instrs.push_back(new jit::JitInstruction(jit::STOR_FLOAT_VAR, instruction->operand1, jit::LOCL));
            break;

          default:
            wcerr << L"Invalid operation" << endl;
            exit(1);
          }
        }
      }
      frame[instruction->operand1] = left;
      break;

    case LBL:			
#ifdef _DEBUG
      wcout << L"LBL: id=" << instruction->operand1 << L", hit_count=" << instruction->operand2 
						<< L", loop_pos=" << loop_iterations.size() <<  endl;
#endif      
			// count hits to label
			if(is_jump && instruction->operand2 < HIT_THRESHOLD) {
				instruction->operand2++;
			}

			// if threshold is reached start recording
			if(instruction->operand2 >= HIT_THRESHOLD) {
				is_recording = true;
				current_jit_label = base_jit_label;
				jit_jump_labels.push(instruction);
#ifdef _DEBUG
        wcout << L"========== JIT START LABEL: id=" << instruction->operand1 << L"==========" << endl;
#endif
			}
      break;

    case JMP:
      switch(instruction->operand2) {
        // unconditional jump
      case JMP_UNCND:
#ifdef _DEBUG
        wcout << L"JMP: unconditional, to=" << instruction->operand1 << endl;
#endif
				if(jit_jump_labels.size() > 0 && 
					 jit_jump_labels.top()->operand1 == instruction->operand1) {					
#ifdef _DEBUG
					wcout << L"========== JIT END LABEL: id=" << jit_jump_labels.top()->operand1 
								<< L"==========" << endl;
#endif
					jit_jump_labels.pop();
				}
				is_jump = jump_table[instruction->operand1] < ip;
        ip = jump_table[instruction->operand1];
        break;
				
        // jump true
      case JMP_TRUE:
#ifdef _DEBUG
        wcout << L"JMP: true, to=" << instruction->operand1 << endl;
#endif
        left = PopValue();
        if(left.type != BOOL_VALUE) {
          wcerr << L">>> Expected a boolean value <<<" << endl;
          exit(1);
        }
        // update ip
        if(left.value.int_value) {
          ip = jump_table[instruction->operand1];
        }
        break;

        // jump false
      case JMP_FALSE:
#ifdef _DEBUG
        wcout << L"JMP: false, to=" << instruction->operand1 << endl;
#endif
        left = PopValue();
        if(left.type != BOOL_VALUE) {
          wcerr << L">>> Expected a boolean value <<<" << endl;
          exit(1);
        }				
        // update ip
        if(!left.value.int_value) {
          ip = jump_table[instruction->operand1];
        }
        break;
      }
      break;
			
    case BIT_AND:
      break;

    case BIT_OR:
      break;

    case EQL:
#ifdef _DEBUG
      wcout << L"EQL" << endl;
#endif
      CALC(L"==", left, right, jit_instrs, is_recording);
      break;

    case NEQL:
#ifdef _DEBUG
      wcout << L"NEQL" << endl;
#endif
      CALC(L"!=", left, right, jit_instrs, is_recording);
      break;

    case GTR:
#ifdef _DEBUG
      wcout << L"GTR" << endl;
#endif
      CALC(L">", left, right, jit_instrs, is_recording);
      break;

    case LES:
#ifdef _DEBUG
      wcout << L"LES" << endl;
#endif
      CALC(L"<", left, right, jit_instrs, is_recording);
      break;

    case GTR_EQL:
#ifdef _DEBUG
      wcout << L"GTR_EQL" << endl;
#endif
      CALC(L">=", left, right, jit_instrs, is_recording);
      break;

    case LES_EQL:
#ifdef _DEBUG
      wcout << L"LES_EQL" << endl;
#endif
      CALC(L"<=", left, right, jit_instrs, is_recording);
      break;

    case ADD:
#ifdef _DEBUG
      wcout << L"ADD" << endl;
#endif
      CALC(L"+", left, right, jit_instrs, is_recording);
      break;

    case SUB:
#ifdef _DEBUG
      wcout << L"SUB" << endl;
#endif
      CALC(L"-", left, right, jit_instrs, is_recording);
      break;

    case MUL:
#ifdef _DEBUG
      wcout << L"MUL" << endl;
#endif
      CALC(L"*", left, right, jit_instrs, is_recording);
      break;

    case DIV:
#ifdef _DEBUG
      wcout << L"DIV" << endl;
#endif
      CALC(L"/", left, right, jit_instrs, is_recording);
      break;

    case MOD:
#ifdef _DEBUG
      wcout << L"MOD" << endl;
#endif
      CALC(L"%", left, right, jit_instrs, is_recording);
      break;

    case DUMP_VALUE:
#ifdef _DEBUG
      wcout << L"DUMP" << endl;
#endif
      left = PopValue();
      switch(left.type) {
      case BOOL_VALUE:
        wcout << L"type=boolean, value=" << (left.value.int_value ? L"true" : L"false") << endl;
        break;

      case INT_VALUE:
        wcout << L"type=integer, value=" << left.value.int_value << endl;
        break;

      case FLOAT_VALUE:
        wcout << L"type=float, value=" << left.value.float_value << endl;
        break;
				
      default:
        wcerr << L"Invalid dump value" << endl;
        exit(1);
      }
      break;
    }
    // update
    instruction = instructions[ip++];
  } 

  delete[] frame;
  frame = NULL;

#ifdef _DEBUG
  wcout << L"==========================" << endl;
  wcout << L"ending stack pos=" << execution_stack_pos << endl;
#endif
}
