/***************************************************************************
 * Runtime system
 *
 * Copyright (c) 2013 Randy Hollines
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

#ifndef __RUNTIME_H__
#define __RUNTIME_H__

#include "classes.h"

namespace runtime {
  /****************************
   * Runtime support structures
   ****************************/
  // size of execution stack
  #define EXECUTION_STACK_SIZE 128
  #define CALL_STACK_SIZE 64
  
  /****************************
 * Call stack frame
 ****************************/
  typedef struct _Frame {
    ExecutableFunction* function;
    size_t ip;
    Value* locals;
    unordered_map<INT_T, size_t>* jump_table;
    bool orphan_return;
  } Frame;
  
  /****************************
   * Execution engine
   ****************************/
  class Runtime {
		ExecutableProgram* program;
    INT_T last_label_id;
		// execution stack and stack pointer
    Value* execution_stack;
		size_t execution_stack_pos;
		// program instructions and jump table
    // vector<Instruction*>* instructions;
	  // unordered_map<INT_T, size_t>* jump_table;
    ExecutableFunction* current_function;
		// loop iteration counts
		stack<INT_T> loop_iterations;
    // call stack
    Frame** call_stack;
    size_t  call_stack_pos;

		// tracing jit variables
#ifndef _NO_JIT
		INT_T current_jit_label;
		bool first_jmp;
		bool is_jump;
#endif
		vector<jit::JitInstruction*> jit_instrs;
		bool is_recording;

    //
    // Calculation stack operations
    //
    Value TopValue() {
			return execution_stack[execution_stack_pos];
		}

		void PushValue(Value &value) {
			if(execution_stack_pos >= EXECUTION_STACK_SIZE) {
				wcerr << ">>> stack bounds exceeded <<<" << endl;
				exit(1);
			}

#ifdef _DEBUG
			wcout << L"  push: type=";
			switch(value.type) {
			case BOOL_VALUE:
				wcout << L"boolean; value=" << (value.value.int_value ? L"true" : L"false") << endl;
				break;
				
			case INT_VALUE:
				wcout << L"integer: value=" << value.value.int_value << endl;
			  break;
				
			case FLOAT_VALUE:
				wcout << L"float: value=" << value.value.float_value << endl;
				break;

      case ARY_VALUE:
				wcout << L"array: address=" << value.value.ptr_value << L", max_bounds=" << ((INT_T*)value.value.ptr_value)[1] << endl;
				break;
				
				// TODO:
			default:
				break;
			}
#endif
			
			execution_stack[execution_stack_pos++] = value;
		}
		
		Value &PopValue() {
			if(execution_stack_pos - 1 < 0) {
				wcerr << ">>> stack bounds exceeded <<<" << endl;
				exit(1);
			}
			
#ifdef _DEBUG
			Value &value = execution_stack[execution_stack_pos  - 1];
			wcout << L"  pop: type=";
			switch(value.type) {
			case BOOL_VALUE:
				wcout << L"boolean; value=" << (value.value.int_value ? L"true" : L"false") << endl;
				break;
				
			case INT_VALUE:
				wcout << L"integer; value=" << value.value.int_value << endl;
			  break;
				
			case FLOAT_VALUE:
				wcout << L"float; value=" << value.value.float_value << endl;
				break;

      case ARY_VALUE:
				wcout << L"array: address=" << value.value.ptr_value << L", max_bounds=" << ((INT_T*)value.value.ptr_value)[1] << endl;
				break;
				
				// TODO:
			default:
				break;
			}
#endif

			return execution_stack[--execution_stack_pos];
		}

    //
    // Calculate array offset
    //
    // TODO: bounds check each dimension
    inline INT_T ArrayIndex(Instruction* instruction, INT_T* array_meta) {
      INT_T index;
      Value value = PopValue();
      switch(value.type) {      
      case INT_VALUE:
        index = value.value.int_value;
        break;

      case FLOAT_VALUE:
        index = (INT_T)value.value.float_value;
        break;

      default:
        wcerr << L">>> Operation requires Integer or Float type <<<" << endl;
        exit(1);
      }

      const INT_T dim = instruction->operand2;
      // TODO: encode array with bounds
      for(INT_T i = 1; i < dim; i++) {
        index *= array_meta[i + 2];
        Value value = PopValue();
        switch(value.type) {      
        case INT_VALUE:
          index += value.value.int_value;
          break;

        case FLOAT_VALUE:
          index += (INT_T)value.value.float_value;
          break;

        default:
          wcerr << L">>> Operation requires Integer or Float type <<<" << endl;
          exit(1);
        }
      }

      if(index >= array_meta[1]) {
        wcerr << L">>> Array index out-of-bounds: index=" << index << L", max_bounds=" << array_meta[1] << L" <<<" << endl;
        exit(1);
      }

      return index;
    }

    //
    // Stack frame operations
    //
    void PushFrame(Frame* frame) {
#ifdef _DEBUG
			wcout << L"pushing frame: address=" << frame << endl;
      assert(call_stack_pos < CALL_STACK_SIZE);
#endif
      call_stack[call_stack_pos++] = frame;
    }

    Frame* PopFrame() {
#ifdef _DEBUG
			wcout << L"popping frame: address=" << call_stack[call_stack_pos  - 1] << endl;
      assert(call_stack_pos - 1 >= 0);
#endif
      return call_stack[--call_stack_pos];
    }

    //
    // Clean up operations
    // 
		void ClearJitInstructions() {
			for(size_t i = 0; i < jit_instrs.size(); i++) {
				jit::JitInstruction* instr = jit_instrs[i];
				delete instr;
				instr = NULL;
			}
			jit_instrs.clear();
			
#ifdef _DEBUG
			wcout << L"============ END RECORDING ============" << endl;
#endif
		}
		
  #ifdef _DEBUG
	  void DumpValue(Value* value, bool is_push) {
		  if(is_push) {
			  wcout << L"  push: ";
		  }
		  else {
			  wcout << L"  pop: ";
		  }
		
		  switch(value->type) {
		  case BOOL_VALUE:
			  wcout << L"boolean=" << (value->value.int_value ? L"true" : L"false") << endl;
			  break;

		  case INT_VALUE:
			  wcout << L"integer=" << value->value.int_value << endl;
			  break;
			
		  case FLOAT_VALUE:
			  wcout << L"float=" << value->value.float_value << endl;
			  break;

      case ARY_VALUE:
				wcout << L"array: address=" << value->value.ptr_value << endl;
				break;

				// TODO:
			default:
				break;
		  }
	  }
  #endif
    
    inline size_t GetLabelOffset(INT_T label) {
      unordered_map<INT_T, size_t>::iterator result = current_function->GetJumpTable()->find(label);
      if(result == current_function->GetJumpTable()->end()) {
        wcerr << ">>> Invalid label identifier <<<" << endl;
        exit(1);
      }
      
      return result->second;
    }
    
    // member operations
    void ClassFunctionCall(Instruction* instruction, size_t &ip, Value* frame);
		void FunctionCall(Instruction* instruction, size_t &ip, Value* frame);
			
   public:
	  Runtime(ExecutableProgram *p, INT_T last_label_id) {
			program = p;
      this->last_label_id = last_label_id;
      // execution stack
      execution_stack = new Value[EXECUTION_STACK_SIZE];
			execution_stack_pos = 0;
      // call stack
      call_stack = new Frame*[CALL_STACK_SIZE];
      call_stack_pos = 0;
      // this should be part of a frame
      this->current_function = p->GetGlobal();
    }
  
    ~Runtime() {
			delete[] execution_stack;
			execution_stack = NULL;

			delete program;
			program = NULL;
    }
  
    void Run();
  };
}

#endif
