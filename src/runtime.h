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

  /****************************
   * Execution engine
   ****************************/
  class Runtime {
		// execution stack and stack pointer
    Value* execution_stack;
		size_t execution_stack_pos;
		// program instructions and jump table
    vector<Instruction*> instructions;
	  unordered_map<INT_T, size_t> jump_table;
		// loop iteration counts
		stack<INT_T> loop_iterations;
		// JIT recording
		bool is_recording;
		bool is_jump;
		INT_T jump_dest;
		INT_T label_start;
		vector<jit::JitInstruction*> jit_instrs;


		Value TopValue() {
			return execution_stack[execution_stack_pos];
		}

		void PushValue(Value &value) {
			if(execution_stack_pos >= EXECUTION_STACK_SIZE) {
				wcerr << ">>> stack bounds exceeded <<<" << endl;
				exit(1);
			}
			
			execution_stack[execution_stack_pos++] = value;
		}
		
		Value &PopValue() {
			if(execution_stack_pos - 1 < 0) {
				wcerr << ">>> stack bounds exceeded <<<" << endl;
				exit(1);
			}
			
			return execution_stack[--execution_stack_pos];
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

				// TODO:
			default:
				break;
		  }
	  }
  #endif
		
    // member operations
    void Add();
	
   public:
	  Runtime(ExecutableProgram *p) {
      this->instructions = p->GetInstructions();
		  this->jump_table = p->GetJumpTable();			
			execution_stack = new Value[EXECUTION_STACK_SIZE];
			execution_stack_pos = 0;
			is_recording = is_jump = false;
    }
  
    ~Runtime() {
    }
  
    void Run();
  };
}

#endif
