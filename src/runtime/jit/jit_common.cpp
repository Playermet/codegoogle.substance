/***************************************************************************
 * Common JIT compiler framework
 *
 * Copyright (c) 2011 Randy Hollines
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
 * - Neither the name of the Objeck Team nor the names of its 
 * contributors may be used to endorse or promote products derived 
 * from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IPLIED WARRANTIES, INCLUDING, BUT NOT 
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

#include "jit_common.h"
#include <string>

using namespace Jit;

long SubExpression::temp_id = 0;

/********************************
 * BasicBlock class
 ********************************/
void BasicBlock::AddExpression(Expression* e, bool can_add) {
  // look for expressions to kill
  multimap<const string, Expression*>::iterator iter;
  for(iter = value_numbers.begin(); iter != value_numbers.end(); iter++) {
    Expression* value_statement = iter->second;
    SubExpression* value_key = value_statement->GetRight();
    // remove dead expression
    if(e->GetLeft()->GetKey() == value_key->GetLeft()->GetKey() || 
       (value_key->GetRight() && e->GetLeft()->GetKey() == value_key->GetRight()->GetKey())) {
      value_numbers.erase(iter);
    }
    // remove dead constant expression
    if(value_key->GetInstruction() && value_key->GetInstruction()->GetType() == LOAD_INT_LIT) {
      if(e->GetLeft()->GetKey() == value_statement->GetLeft()->GetKey()) {
	value_numbers.erase(iter);
      }
    }
  }

  // propagation constants      
  SubExpression* statement_expression = e->GetRight();
  if(statement_expression) {
    // constant propagation
    for(iter = value_numbers.begin(); iter != value_numbers.end(); iter++) {
      Expression* value_statement = iter->second;
      SubExpression* value_key = value_statement->GetRight();
      if(value_key->GetInstruction() && 
    	 value_key->GetInstruction()->GetType() == LOAD_INT_LIT) {
    	// single-term expression
    	if(statement_expression->GetInstruction()) {
    	  if(statement_expression->GetLeft()->GetKey() == value_statement->GetLeft()->GetKey()) {
    	    e->SetRight(value_key);
    	  }
    	}
    	// two-term expression
    	else {
    	  if(statement_expression->GetLeft()->GetKey() == value_statement->GetLeft()->GetKey()) {
    	    statement_expression->SetLeft(value_key);
    	  }
    	  if(statement_expression->GetRight() && 
    	     statement_expression->GetRight()->GetKey() == value_statement->GetLeft()->GetKey()) {
    	    statement_expression->SetRight(value_key);
    	  }
    	}
      }
    }
      
    // look to reuse sub-expression
    multimap<const string, Expression*>::iterator result = value_numbers.find(statement_expression->GetKey());
    if(result != value_numbers.end()) {
      e->SetRight(result->second->GetLeft());
    }
      
    // TODO: strength reduction, simplification
    if(!statement_expression->GetInstruction()) {
      SubExpression* left = statement_expression->GetLeft();
      SubExpression* right = statement_expression->GetRight();
      
      if(left->GetInstruction()->GetType() == LOAD_INT_LIT &&
	 right->GetInstruction()->GetType() == LOAD_INT_LIT) {
	StackInstr* instr = statement_expression->GetOperation()->GetInstruction();
	switch(instr->GetType()) {
	case ADD_INT: {
	  StackInstr* instr = new StackInstr(-1, LOAD_INT_LIT, 
					     left->GetInstruction()->GetOperand() + 
					     right->GetInstruction()->GetOperand());
	  new_instrs.push_back(instr);
	  e->SetRight(jit_ir->MakeSubExpression(instr));
	}
	  break;

	case SUB_INT: {
	  StackInstr* instr = new StackInstr(-1, LOAD_INT_LIT, 
					     left->GetInstruction()->GetOperand() - 
					     right->GetInstruction()->GetOperand());
	  new_instrs.push_back(instr);
	  e->SetRight(jit_ir->MakeSubExpression(instr));
	}
	  break;
	
	case MUL_INT: {
	  StackInstr* instr = new StackInstr(-1, LOAD_INT_LIT, 
					     left->GetInstruction()->GetOperand() * 
					     right->GetInstruction()->GetOperand());
	  new_instrs.push_back(instr);
	  e->SetRight(jit_ir->MakeSubExpression(instr));
	}
	  break;

	default:
	  break;
	}
      }
    }
    // update value numbers
    value_numbers.insert(pair<const string, Expression*>(e->GetRight()->GetKey(), e));
  }
  
  // add statement
  if(can_add) {
    expressions.push_back(e);
  }
}

/********************************
 * JitIR class
 ********************************/
void JitIR::CreateBasicBlocks() {
  long instr_index = 0;
  
  // skip parameters
  bool is_param_store = true;
  while(is_param_store && instr_index < method->GetInstructionCount()) {
    StackInstr* instr = method->GetInstruction(instr_index);
    switch(instr->GetType()) {
    case STOR_INT_VAR:
    case STOR_FLOAT_VAR:
    case STOR_FUNC_VAR:
      instr_index++;
      break;
      
    default:
      is_param_store = false;
    }
  }

  // process instructions
  while(instr_index < method->GetInstructionCount()) {
    StackInstr* instr = method->GetInstruction(instr_index++);
    
    switch(instr->GetType()) {
      // load literal
    case LOAD_INT_LIT:
      working_stack.push(MakeSubExpression(instr));
      break;
      
      // float literal
    case LOAD_FLOAT_LIT: {
      // look for duplicates
      bool found = false;
      const double value = instr->GetFloatOperand();
      unsigned long index = 0;
      for(; !found && index < floats.size(); index++) {
	if(floats[index] == value) {
	  found = true;
	}
      }
      // update index
      if(found) {
	working_stack.push(MakeSubExpression(instr, (long)index));
      } 
      else {
	floats.push_back(instr->GetFloatOperand());
	working_stack.push(MakeSubExpression(instr, (long)(floats.size() - 1)));
      }
    }
      break;
      
      // load self
    case LOAD_INST_MEM:
    case LOAD_CLS_MEM:
      working_stack.push(MakeSubExpression(instr));
      break;
      
      // load variable
    case LOAD_INT_VAR:
    case LOAD_FLOAT_VAR:
    case LOAD_FUNC_VAR:
      if(instr->GetOperand2() != LOCL) {
	working_stack.pop();
      }
      working_stack.push(MakeSubExpression(instr));
      break;
    
      // store value
    case STOR_INT_VAR:
    case STOR_FLOAT_VAR:
    case STOR_FUNC_VAR: {
      SubExpression* left = MakeSubExpression(instr);
      if(instr->GetOperand2() != LOCL) {
	working_stack.pop();
      }
      SubExpression* right = working_stack.top(); 
      working_stack.pop();
      cur_block->AddExpression(new Expression(left, right));
    }
      break;
      
    case RTRN: {
      SubExpression* left = MakeSubExpression(instr);
      if(working_stack.size() > 0) {
	SubExpression* right = working_stack.top(); 
	working_stack.pop();
	left->GetInstruction()->SetOperand(temp_id++);
	cur_block->AddExpression(new Expression(left, right));
      }
      else {
	cur_block->AddExpression(new Expression(left));
      }
      NewBlock();
    }
      break;
      
      // copy value
    case COPY_INT_VAR:
    case COPY_FLOAT_VAR:
      break;
      
      // mathematical
    case AND_INT:
    case OR_INT:
    case ADD_INT:
    case SUB_INT:
    case MUL_INT:
    case DIV_INT:
    case MOD_INT:
    case BIT_AND_INT:
    case BIT_OR_INT:
    case BIT_XOR_INT:
    case LES_INT:
    case GTR_INT:
    case LES_EQL_INT:
    case GTR_EQL_INT:
    case EQL_INT:
    case NEQL_INT:
    case SHL_INT:
    case SHR_INT: {
      SubExpression* left = working_stack.top(); working_stack.pop();
      if(!left->GetInstruction()) {
	left = ProcessSubExpressionCalculation(left);
      }

      SubExpression* right = working_stack.top(); working_stack.pop();
      if(!right->GetInstruction()) {
	right = ProcessSubExpressionCalculation(right);
      }           
      
      working_stack.push(MakeSubExpression(left, MakeSubExpression(instr), right));
    }
      break;
      
    case ADD_FLOAT:
    case SUB_FLOAT:
    case MUL_FLOAT:
    case DIV_FLOAT:
      break;

    case LES_FLOAT:
    case GTR_FLOAT:
    case LES_EQL_FLOAT:
    case GTR_EQL_FLOAT:
    case EQL_FLOAT:
    case NEQL_FLOAT:
      break;
      
    case MTHD_CALL: {
      StackMethod* called_method = program->GetClass(instr->GetOperand())->GetMethod(instr->GetOperand2());
      if(called_method) {
#ifdef _DEBUG
	assert(called_method);
	assert((long)working_stack.size() >= called_method->GetParamCount());
#endif
	Expression* call_expr = new Expression(MakeSubExpression(instr));
	call_expr->SetIndex(instr_index);
	long param_count = called_method->GetParamCount() + 1;
	while(param_count--) {
	  // create temp lhs
	  StackInstr* instr = new StackInstr(-1, STOR_INT_VAR, temp_id++);
	  instr->SetOperand2(LOCL);
	  new_instrs.push_back(instr);
	  SubExpression* left = MakeSubExpression(instr, true);
	  // process rhs expression
	  SubExpression* right = working_stack.top(); 
	  working_stack.pop();
	  left->GetInstruction()->SetOperand(temp_id++);
	  // add parameter
	  Expression* expression = new Expression(left, right);
	  cur_block->AddExpression(expression, false);
	  call_expr->AddParameter(expression);
	}
	// add expression
	cur_block->AddExpression(call_expr);
	NewBlock();
      }
    }
      break;

      // TODO: implement
    case DYN_MTHD_CALL:
      break;
      
    case NEW_INT_ARY:
      break;

    case NEW_FLOAT_ARY:
      break;
      
    case NEW_OBJ_INST:
      break;
      
    case THREAD_JOIN: 
      break;

    case THREAD_SLEEP: 
      break;
      
    case CRITICAL_START: 
      break;
      
    case CRITICAL_END: 
      break;
      
    case TRAP: {
      Expression* trap_expr = new Expression(MakeSubExpression(instr));
      trap_expr->SetIndex(instr_index);    
      long param_count = instr->GetOperand();
      while(param_count--) {  
	// create temp lhs
	StackInstr* instr = new StackInstr(-1, STOR_INT_VAR, temp_id++);
	instr->SetOperand2(LOCL);
	new_instrs.push_back(instr);
	SubExpression* left = MakeSubExpression(instr, true);
	// process rhs expression
	SubExpression* right = working_stack.top(); 
	working_stack.pop();
	left->GetInstruction()->SetOperand(temp_id++);
	trap_expr->AddParameter(new Expression(left, right));
      }
      // add expression
      cur_block->AddExpression(trap_expr);
      NewBlock();
    }
      break;
      
    case TRAP_RTRN:
      cout << ">> TRAP_RTRN <<" << endl;
      break;
      
    case STOR_BYTE_ARY_ELM:
    case STOR_INT_ARY_ELM:{
      SubExpression* array = working_stack.top(); 
      working_stack.pop();
      // create expression
      SubExpression* left = MakeSubExpression(instr);      
      // add array info
      left->AddSubExpression(array);
      for(long i = 0; i < instr->GetOperand(); i++) {
	SubExpression* sub_expr = working_stack.top(); 
	working_stack.pop();
	left->AddSubExpression(sub_expr);
      }
      // store array element
      SubExpression* right = working_stack.top(); 
      working_stack.pop();
      cur_block->AddExpression(new Expression(left, right));
    }
      break;

    case STOR_FLOAT_ARY_ELM:
      break;

    case SWAP_INT:
      break;

    case POP_INT:
    case POP_FLOAT: 
      break;

    case FLOR_FLOAT:
      break;

    case CEIL_FLOAT:
      break;
      
    case F2I:
      break;

    case I2F:
      break;

    case OBJ_TYPE_OF: 
      break;
      
    case OBJ_INST_CAST: 
      break;
      
    case LOAD_BYTE_ARY_ELM:
    case LOAD_INT_ARY_ELM: {
      SubExpression* array = working_stack.top(); 
      working_stack.pop();
      // create expression
      SubExpression* array_expr = MakeSubExpression(instr);      
      // add array info
      array_expr->AddSubExpression(array);
      for(long i = 0; i < instr->GetOperand(); i++) {
	SubExpression* sub_expr = working_stack.top(); 
	working_stack.pop();
	array_expr->AddSubExpression(sub_expr);
      }
      // push sub-expression
      working_stack.push(array_expr);
    }
      break;

    case LOAD_FLOAT_ARY_ELM:
      break;
      
    case JMP: {
      // conditonal
      if(instr->GetOperand2() > -1) {
	SubExpression* left = MakeSubExpression(instr);
	SubExpression* right = working_stack.top(); 
	working_stack.pop();
	cur_block->AddExpression(new Expression(left, right));
	NewBlock();
      }
      // uncondtional
      else {	
	NewBlock();
	cur_block->AddExpression(new Expression(MakeSubExpression(instr)));
      }      
    }
      break;
      
    case LBL:
      NewBlock();
      labels.insert(pair<long, BasicBlock*>(instr->GetOperand(), cur_block));
      cur_block->AddExpression(new Expression(MakeSubExpression(instr)));
      break;
      
    default: {
      InstructionType error = (InstructionType)instr->GetType();
      cerr << "Unknown instruction: " << error << "!" << endl;
      exit(1);
    }
      break;
    }
  }
}

SubExpression* JitIR::ProcessSubExpressionCalculation(SubExpression* s) 
{
  StackInstr* instr = new StackInstr(-1, STOR_INT_VAR, temp_id++);
  instr->SetOperand2(LOCL);
  new_instrs.push_back(instr);
  SubExpression* left = MakeSubExpression(instr, true);
  cur_block->AddExpression(new Expression(left, s));
  
  return left;
}
