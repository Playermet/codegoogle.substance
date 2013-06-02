/***************************************************************************
 * Instruction emitter
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

#include "emitter.h"

vector<Instruction*> Emitter::instruction_factory;

/****************************
 * TODO: doc
 ****************************/
vector<Instruction*> Emitter::Emit()
{
#ifdef _DEBUG
  wcout << L"\n---------- Emitting ---------" << endl;
#endif
  
  return EmitBlock(parsed_program);
}

/****************************
 * TODO: doc
 ****************************/
vector<Instruction*> Emitter::EmitBlock(StatementList* block_statements)
{
  vector<Instruction*> block_instructions;
  vector<Statement*> statements = block_statements->GetStatements();
  for(size_t i = 0; i < statements.size(); ++i) {
    Statement* statement = statements[i];
    switch(statement->GetStatementType()) {
    case ASSIGNMENT_STATEMENT:
      EmitAssignment(static_cast<Assignment*>(statement), block_instructions);
      break;
      
    default:
      // TODO: report error
      break;
    }
  }
  
  block_instructions.push_back(MakeInstruction(RTRN));
  return block_instructions;
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitAssignment(Assignment* assignment, vector<Instruction*> &block_instructions)
{
  EmitExpression(assignment->GetExpression(), block_instructions);
  EmitReference(assignment->GetReference(), true, block_instructions);
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitReference(Reference* reference, bool is_store, vector<Instruction*> &block_instructions)
{
  wstring name = reference->GetName();
  if(is_store) {
#ifdef _DEBUG
    wcout << L"Emitting store: reference=" << reference->GetName() << endl;
#endif
    block_instructions.push_back(MakeInstruction(STOR_VAR, name));
  }
  else {
#ifdef _DEBUG
    wcout << L"Emitting load: reference=" << reference->GetName() << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_VAR, name));
  }
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitExpression(Expression* expression, vector<Instruction*> &block_instructions)
{
  switch(expression->GetExpressionType()) {
  case CHAR_LIT_EXPR:
    // TODO: implement
    break;
    
  case INT_LIT_EXPR:
#ifdef _DEBUG
    wcout << L"Emitting integer literal: value=" 
          << static_cast<IntegerLiteral*>(expression)->GetValue() << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_INT_LIT, static_cast<IntegerLiteral*>(expression)->GetValue()));
    break;
    
  case FLOAT_LIT_EXPR:
#ifdef _DEBUG
    wcout << L"Emitting float literal: value=" 
          << static_cast<FloatLiteral*>(expression)->GetValue() << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_FLOAT_LIT, static_cast<FloatLiteral*>(expression)->GetValue()));
    break;

  case BOOLEAN_LIT_EXPR:
    break;

  case AND_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '&&'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(AND));
  }
    break;

  case OR_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '||'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(OR));
  }
    break;
    
  case EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '=='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(EQL));
  }
    break;

  case NEQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '!='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(NEQL));
  }
    break;

  case LES_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '<'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(LES));
  }
    break;

  case GTR_EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '>='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(GTR_EQL));
  }
    break;

  case LES_EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '<='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(LES_EQL));
  }
    break;
    
  case GTR_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '>'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(GTR));
  }
    break;

  case ADD_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '+'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(ADD));
  }
    break;
    
  case SUB_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '-'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(SUB));
  }
    break;

  case MUL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '*'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(MUL));
  }
    break;

  case DIV_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '/'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(DIV));
  }
    break;
    
  case MOD_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions);
#ifdef _DEBUG
    wcout << L"Emitting operator: '%'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(MOD));
  }
    break;
    
  default:
    // TODO: error
    break;
  }
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::ClearInstructions() {
  while(!instruction_factory.empty()) {
    Instruction* tmp = instruction_factory.front();
    instruction_factory.erase(instruction_factory.begin());
    // delete
    delete tmp;
    tmp = NULL;
  }
}
