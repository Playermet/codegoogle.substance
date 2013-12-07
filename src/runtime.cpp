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
    left = PopValue();                                      \
    right = PopValue();                                     \
    if(left.klass) {                                        \
      Operation oper = left.klass->GetOperation(name);      \
      (*oper)(left, right, left, jit_instrs, is_recording); \
      PushValue(left);                                      \
    }                                                       \
    else {                                                  \
      wcerr << L">>> invalid operation <<<" << endl;        \
      exit(1);                                              \
    }                                                       \
  }

/****************************
 * TODO: doc
 ****************************/
void Runtime::Run()
{
#ifdef _DEBUG
  wcout << L"========== Executing Code =========" << endl;
#endif

  // setup locals
  const long local_size = program->GetGlobal()->GetLocalCount();
  Value* locals = new Value[local_size + 1];
  memset(locals, local_size * sizeof(Value), 0);
  
  // initialize 'self'
  locals[0].type = CLS_VALUE;
  locals[0].value.ptr_value = NULL;
  
  // initialize JIT tracing
#ifndef _NO_JIT
  stack<Instruction*> jit_jump_labels;
  INT_T jit_base_label = last_label_id + 1;
  INT_T jit_end_label;
  is_jump = false;
#endif
  
  // start execution
  is_recording = false;
  Value left, right;
  size_t ip = 0;  
  bool halt = false;
  do {
    Instruction* instruction = current_function->GetInstructions()->at(ip++);
    switch(instruction->type) {
    case RTRN: {
      if(call_stack_pos == 0) {
        halt = true;
      }
      else {
        Frame* frame = PopFrame();
        ip = frame->ip;
        current_function = frame->function;
        locals = frame->locals;
        // clean up orphan return value
        if(frame->orphan_return) {
          PopValue();
        }

        delete frame;
        frame = NULL;
#ifdef _DEBUG
      wcout << L"=== RTRN ===" << endl;
#endif
      }
    }
      break;
			
    case CALL_CLS_FUNC:
#ifdef _DEBUG
      wcout << L"=== CALL_CLS_FUNC: class='" << instruction->operand5 << L"', function=" << instruction->operand6 << L" ===" << endl;
#endif
      ClassFunctionCall(instruction, ip, locals);
			break;

		case CALL_FUNC:
#ifdef _DEBUG
      wcout << L"=== CALL_FUNC: function='" << instruction->operand5 << L"' ===" << endl;
#endif
      FunctionCall(instruction, ip, locals);
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
      left.value.float_value = instruction->operand4;
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
      left = locals[instruction->operand1];
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

    case STOR_ARY_VAR: {
      left = locals[instruction->operand1];
      if(left.type != ARY_VALUE) {
        wcerr << L">>> Operation requires Integer or Float type <<<" << endl;
      }
      
      INT_T* array_meta = (INT_T*)left.value.ptr_value;
      INT_T index = ArrayIndex(instruction, array_meta, true);
      Value* array = (Value*)(array_meta + array_meta[0]);
#ifdef _DEBUG
      wcout << L"STOR_ARY_VAR: id=" << instruction->operand1 << L", native_offset=" << index << endl;
#endif
      array[index] = PopValue();

      if(is_recording) {
        jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_VAR, instruction->operand1, jit::LOCL));
        
        switch(array[index].type) {      
        case INT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::STOR_INT_ARY_ELM, instruction->operand2));
          break;

        case FLOAT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::STOR_FLOAT_ARY_ELM, instruction->operand2));
          break;
          
        default:
          break;
        }
      }        
    }
      break;

    case STOR_VAR:
#ifdef _DEBUG
      wcout << L"STOR_VAR: id=" << instruction->operand1 << endl;
#endif
      left = PopValue();
      // copy value
      locals[instruction->operand1].klass = left.klass;
      switch(left.type) {
      case BOOL_VALUE:
        locals[instruction->operand1].type = BOOL_VALUE;
        locals[instruction->operand1].value.int_value = left.value.int_value;
        break;
        
      case INT_VALUE:
        locals[instruction->operand1].type = INT_VALUE;
        locals[instruction->operand1].value.int_value = left.value.int_value;
        break;

      case FLOAT_VALUE:        
        locals[instruction->operand1].type = FLOAT_VALUE;
        locals[instruction->operand1].value.float_value = left.value.float_value;
        break;      

      case ARY_VALUE:
        locals[instruction->operand1].type = ARY_VALUE;
        locals[instruction->operand1].value.ptr_value = left.value.ptr_value;
        break;

      case UNINIT_VALUE:
        locals[instruction->operand1].type = UNINIT_VALUE;
        break;
        
      default:
        wcerr << L"Invalid type" << endl;
        exit(1);
      }
      
      // record JIT instructions
      if(is_recording) {
        // ensure the type hasn't changed
        if(locals[instruction->operand1].type == left.type) {
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
      break;

    case LOAD_ARY_VAR: {
      left = locals[instruction->operand1];
      if(left.type != ARY_VALUE) {
        wcerr << L">>> Operation requires Integer or Float type <<<" << endl;
      }
      INT_T* array_meta = (INT_T*)left.value.ptr_value;
      INT_T index = ArrayIndex(instruction, array_meta, false);
      Value* array = (Value*)(array_meta + array_meta[0]);
#ifdef _DEBUG
      wcout << L"LOAD_ARY_VAR: id=" << instruction->operand1 << L", native_offset=" << index << endl;
#endif
      PushValue(array[index]);
      
      if(is_recording) {
        jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_VAR, instruction->operand1, jit::LOCL));
        
        switch(array[index].type) {      
        case INT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_INT_ARY_ELM, instruction->operand2));
          break;

        case FLOAT_VALUE:
          jit_instrs.push_back(new jit::JitInstruction(jit::LOAD_FLOAT_ARY_ELM, instruction->operand2));
          break;
          
        default:
          break;
        }
      }
    }
      break;

    case LBL:			
#ifdef _DEBUG
      wcout << L"LBL: id=" << instruction->operand1 << L", hit_count=" << instruction->operand2 
            << L", loop_pos=" << loop_iterations.size() <<  endl;
#endif 

      // tracing jit code
#ifndef _NO_JIT
      // count hits to label
      if(is_jump && instruction->operand2 < HIT_THRESHOLD) {
        instruction->operand2++;
        is_jump = false;
      }
      // if threshold is reached start recording
      if(!is_recording && instruction->operand2 >= HIT_THRESHOLD) {
        if(instruction->native_code) {
#ifdef _DEBUG	
          wcout << L"### NATIVE CODE: id=" << instruction->operand1 << L" ###" << endl;
#endif					
          jit::jit_fun_ptr jit_fun = instruction->native_code;
          const INT_T guard_label = (*jit_fun)(locals, NULL, NULL);
          // TODO: manage error codes
          ip = guard_label == -1 ? instruction->operand3 : guard_label;

        }
        else {
#ifdef _DEBUG
          wcout << L"### JIT START LABEL: id=" << instruction->operand1 << L" ###" << endl;
#endif
          is_recording = first_jmp = true;
          jit_instrs.push_back(new jit::JitInstruction(jit::LBL, instruction->operand1));
          current_jit_label = jit_base_label;
          jit_jump_labels.push(instruction);
        }
        is_jump = false;
      }
#endif
      break;

    case JMP:
      switch(instruction->operand2) {
        // unconditional jump
      case JMP_UNCND:
#ifdef _DEBUG
        wcout << L"JMP: unconditional, to=" << instruction->operand1 << endl;
#endif

        // tracing jit code
#ifndef _NO_JIT
        if(jit_jump_labels.size() > 0 && jit_jump_labels.top()->operand1 == instruction->operand1) {
          Instruction* jit_start_label = jit_jump_labels.top();
          jit_jump_labels.pop();

          if(!jit_start_label->native_code) {
#ifdef _DEBUG
            wcout << L"### JIT END LABEL: id=" << jit_start_label->operand1 << L" ###" << endl;
#endif
            // add ending code for JIT compiler
            jit_instrs.push_back(new jit::JitInstruction(jit::JMP, jit_start_label->operand1, JMP_UNCND));
            jit_instrs.push_back(new jit::JitInstruction(jit::LBL, (INT_T)jit_end_label));
            // compile into native code and execute
            jit::JitCompiler compiler(jit_instrs, current_function->GetJumpTable(), jit_end_label);
            jit::jit_fun_ptr jit_fun = compiler.Compile();
            if(!jit_fun) {
              wcerr << L">>> Unable to JIT compile trace <<<" << endl;
              exit(1);
            }
            jit_start_label->native_code = jit_fun;	
            jit_start_label->operand3 = (INT_T)(ip + 1);
            // reset
            is_recording = first_jmp = false;
            jit_base_label = last_label_id;
            ClearJitInstructions();
          }
        }
        else {
          is_jump = GetLabelOffset(instruction->operand1) < ip;
        }				
#endif
        ip = GetLabelOffset(instruction->operand1);
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
          ip = GetLabelOffset(instruction->operand1);
        }

        // tracing jit code
#ifndef _NO_JIT
        // record JIT instructions
        if(is_recording) {
          if(first_jmp) {
            jit_instrs.push_back(new jit::JitInstruction(jit::JMP, instruction->operand1, left.value.int_value, -1));
            jit_end_label = instruction->operand1;
            first_jmp = false;
          }
          else {
            jit_base_label++;
            const bool jump_taken = left.value.int_value ? true : false;
            jit_instrs.push_back(new jit::JitInstruction(jit::JMP, jit_base_label, jump_taken, (long)ip));
            jit_instrs.push_back(new jit::JitInstruction(jit::LBL, jit_base_label));            
          }
        }
#endif
        break;

        // jump false
      case JMP_FALSE:
#ifdef _DEBUG
        wcout << L"JMP: false, to=" << instruction->operand1 << endl;
        if(instruction->operand1 == -2147483646) {
          wcout << L"Foo" << endl;
        }
#endif
        left = PopValue();
        if(left.type != BOOL_VALUE) {
          wcerr << L">>> Expected a boolean value <<<" << endl;
          exit(1);
        }				
        // update ip
        if(!left.value.int_value) {
          ip = GetLabelOffset(instruction->operand1);
        }

        // tracing jit code
#ifndef _NO_JIT
        // record JIT instructions
        if(is_recording) {
          if(first_jmp) {
            jit_instrs.push_back(new jit::JitInstruction(jit::JMP, instruction->operand1, !left.value.int_value, -1));
            jit_end_label = instruction->operand1;
            first_jmp = false;
          }
          else {
            jit_base_label++;
            const bool jump_taken = left.value.int_value ? true : false;
            jit_instrs.push_back(new jit::JitInstruction(jit::JMP, jit_base_label, !jump_taken, (long)ip));
            jit_instrs.push_back(new jit::JitInstruction(jit::LBL, jit_base_label));            
          }
        }
#endif
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

      case UNINIT_VALUE:
        wcout << L"type=uninit, value=Nil" << endl;
        break;

      default:
        wcerr << L"Invalid dump value" << endl;
        exit(1);
      }
      break;
    }    
  } 
  while(!halt);

  delete[] locals;
  locals = NULL;

#ifdef _DEBUG
  wcout << L"==========================" << endl;
  wcout << L"ending stack pos=" << execution_stack_pos << endl;
#endif
}

void Runtime::ClassFunctionCall(Instruction* instruction, size_t &ip, Value* locals)
{
  RuntimeClass* klass = Classes::Instance()->GetClass(instruction->operand5);
  if(!klass) {
    wcerr << L">>> Unknown class: name=" << instruction->operand5 << L" <<<" << endl;
    exit(1);
  }
  Function func = klass->GetFunction(instruction->operand6);
  if(!func) {
    wcerr << L">>> Unknown function: name='" << instruction->operand5 << L"' <<<" << endl;
    exit(1);
  }
  Value self = locals[0];
  (*func)(self, execution_stack, execution_stack_pos, instruction->operand1);
}

void Runtime::FunctionCall(Instruction* instruction, size_t &ip, Value* locals)
{
  ExecutableFunction* callee = program->GetFunction(instruction->operand5);
  if(!callee) {
    wcerr << L">>> Unknown function: name='" << instruction->operand5 << L"' <<<" << endl;
    exit(1);
  }  
  if(callee->GetParameterCount() != instruction->operand1) {
    wcerr << L">>> Incorrect number of calling parameters <<<" << endl;
    exit(1);
  }

  // push stack frame
  Frame* frame = new Frame;
  frame->ip = ip;
  frame->function = current_function;
  frame->locals = locals;
  // function returns an orphan value
  if(callee->ReturnsValue() && !instruction->operand2) {
    frame->orphan_return = true;
  }
  else {
    frame->orphan_return = false;
  }
  PushFrame(frame);
  
  // initialize new frame
  if(is_recording) {
    jit_instrs.push_back(new jit::JitInstruction(jit::MTHD_CALL, current_function));
  }
  current_function = callee;
  locals = new Value[current_function->GetLocalCount()];
  ip = 0;
}
