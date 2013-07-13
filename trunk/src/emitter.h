/***************************************************************************
 * Instruction emitter
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

#ifndef __EMITTER_H__
#define __EMITTER_H__

#include "common.h"
#include "tree.h"
#include <limits.h>

/****************************
 * Translate trees to instructions
 ****************************/

namespace compiler {
  class Emitter {
    ParsedProgram* parsed_program;
    static vector<Instruction*> instruction_factory;
	  INT_T start_label_id;
    INT_T end_label_id;
    bool returns_value;
    
    INT_T NextEndId() {
      return end_label_id++;
    }
    
    INT_T NextStartId() {
      return start_label_id++;
    }
    
    ExecutableFunction* EmitFunction(ParsedFunction* parsed_function);
    void EmitFunction(StatementList* block_statements, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table, set<size_t> &leaders);
    void EmitBlock(StatementList* block_statements, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table);
		void EmitFunctionCall(Reference* reference, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table);
    void EmitIfElse(IfElse* if_else, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table);    
	  void EmitWhile(While* if_while, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table);
    void EmitAssignment(Assignment* assignment, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table);
    void EmitReference(Reference* reference, bool is_store, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table);
    void EmitExpression(Expression* expression, vector<Instruction*>* block_instructions, unordered_map<long, size_t>* jump_table);
  
   public:
    Emitter(ParsedProgram* parsed_program) {
      this->parsed_program = parsed_program;
		  start_label_id = 0;
      end_label_id = INT_MIN;
    }
  
    ~Emitter() {
    }

    static Instruction* MakeInstruction(InstructionType type) {
      Instruction* instruction = new Instruction;
      instruction->type = type;
      instruction_factory.push_back(instruction);
    
      return instruction;
    }
	
    static Instruction* MakeInstruction(InstructionType type, int operand) {
      Instruction* instruction = new Instruction;
      instruction->type = type;
      instruction->operand1 = operand;
      instruction_factory.push_back(instruction);
    
      return instruction;
    }

    static Instruction* MakeInstruction(InstructionType type, int operand1, int operand2) {
      Instruction* instruction = new Instruction;
      instruction->type = type;
      instruction->operand1 = operand1;
      instruction->operand2 = operand2;
      instruction_factory.push_back(instruction);
    
      return instruction;

    }
  
    static Instruction* MakeInstruction(InstructionType type, double operand) {
      Instruction* instruction = new Instruction;
      instruction->type = type;
      instruction->operand4 = operand;
      instruction_factory.push_back(instruction);
    
      return instruction;
    }

		static Instruction* MakeInstruction(InstructionType type, int operand1, const wstring &operand5) {
      Instruction* instruction = new Instruction;
      instruction->type = type;
      instruction->operand1 = operand1;
      instruction->operand5 = operand5;
      instruction_factory.push_back(instruction);
			
      return instruction;
    }

    static Instruction* MakeInstruction(InstructionType type, int operand1, const wstring &operand5, const wstring &operand6) {
      Instruction* instruction = new Instruction;
      instruction->type = type;
      instruction->operand1 = operand1;
      instruction->operand5 = operand5;
      instruction->operand6 = operand6;
      instruction_factory.push_back(instruction);
			
      return instruction;
    }
		
    INT_T GetLastLabelId() {
      return 0;
    }
		
    static void ClearInstructions();
  
    ExecutableProgram* Emit();
  };
 }

#endif
