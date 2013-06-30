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

using namespace compiler;

vector<Instruction*> Emitter::instruction_factory;

/****************************
 * TODO: doc
 ****************************/
ExecutableProgram* Emitter::Emit()
{
#ifdef _DEBUG
  wcout << L"\n---------- Emitting Instructions ---------" << endl;
#endif
  
	vector<Instruction*> block_instructions;
	unordered_map<long, size_t> jump_table;
  set<size_t> leaders;
	
	EmitFunctionMethod(parsed_program, block_instructions, jump_table, leaders);
  block_instructions.push_back(MakeInstruction(RTRN));

	return new ExecutableProgram(block_instructions, jump_table, leaders);
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitFunctionMethod(StatementList* block_statements, vector<Instruction*> &block_instructions,
                                 unordered_map<long, size_t> &jump_table, set<size_t> &leaders)
{
  EmitBlock(parsed_program, block_instructions, jump_table);

  // create CFG
  leaders.insert(0);
  for(size_t i = 0; i < block_instructions.size(); ++i) {
    if(block_instructions[i]->type == LBL) {
      leaders.insert(i);      
    }
    else if(block_instructions[i]->type == JMP) {
      leaders.insert(i + 1);
    }
  }

#ifdef _DEBUG
  wcout << L"Leaders" << endl;
  set<size_t>::iterator iter = leaders.begin(); 
  for(;iter != leaders.end(); ++iter) {
    wcout << L"  " << *iter << endl;
  }
#endif
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitBlock(StatementList* block_statements, vector<Instruction*> &block_instructions,
												unordered_map<long, size_t> &jump_table)
{
  vector<Statement*> statements = block_statements->GetStatements();
  for(size_t i = 0; i < statements.size(); i++) {
    Statement* statement = statements[i];
    switch(statement->GetStatementType()) {
    case ASSIGNMENT_STATEMENT:
      EmitAssignment(static_cast<Assignment*>(statement), block_instructions, jump_table);
      break;
			
    case IF_ELSE_STATEMENT:
      EmitIfElse(static_cast<IfElse*>(statement), block_instructions, jump_table);
      break;
      
		case WHILE_STATEMENT:
			EmitWhile(static_cast<While*>(statement), block_instructions, jump_table);
      break;

    case DUMP_STATEMENT:
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"dump value" << endl;
#endif
      EmitExpression(static_cast<Dump*>(statement)->GetExpression(), block_instructions, jump_table);
      block_instructions.push_back(MakeInstruction(DUMP_VALUE));
      break;
      
    default:
      // TODO: report error
      break;
    }
  }
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitIfElse(IfElse* if_else, vector<Instruction*> &block_instructions, 
                         unordered_map<long, size_t> &jump_table)
{
  const long end_label = NextEndId();
  long next_label = NextStartId();
  
  // emit test
	EmitExpression(if_else->GetExpression(), block_instructions, jump_table);

  // basic 'if' statement
  vector<IfElse*> else_ifs = if_else->GetElseIfs();
  StatementList* else_block = if_else->GetElseBlock();
  if(else_ifs.size() == 0 && !else_block) {
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"jump false: id=" << end_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(JMP, (int)end_label, 0));
    // emit 'if' block
    EmitBlock(if_else->GetIfBlock(), block_instructions, jump_table);
  }
  else {
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"jump false: id=" << next_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(JMP, (int)next_label, 0));
    // emit 'if' block
    EmitBlock(if_else->GetIfBlock(), block_instructions, jump_table);    
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"jump: id=" << end_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(JMP, (int)end_label, -1));
  }
  
  // 'if-else' blocks
  if(else_ifs.size() > 0) {
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"label: id=" << next_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(LBL, (int)next_label, 0));
    jump_table.insert(pair<long, size_t>(next_label, block_instructions.size() - 1));
    
    for(size_t i = 0; i < else_ifs.size(); i++) {
      IfElse* else_if = else_ifs[i];
      // emit test
      EmitExpression(else_if->GetExpression(), block_instructions, jump_table);  
#ifdef _DEBUG
      wcout << block_instructions.size() << L": " << L"jump false: id=" << (next_label + 1) << endl;
#endif
      block_instructions.push_back(MakeInstruction(JMP, (int)next_label + 1, 0));
      
      // emit 'if' block
      EmitBlock(else_if->GetIfBlock(), block_instructions, jump_table);    
#ifdef _DEBUG
      wcout << block_instructions.size() << L": " << L"jump: id=" << end_label << endl;
#endif
      // jump to end
      block_instructions.push_back(MakeInstruction(JMP, (int)end_label, -1));
      
      next_label = NextStartId();
#ifdef _DEBUG
      wcout << block_instructions.size() << L": " << L"label: id=" << next_label << endl;
#endif
      block_instructions.push_back(MakeInstruction(LBL, (int)next_label, 0));
      jump_table.insert(pair<long, size_t>(next_label, block_instructions.size() - 1));      
    }
  }
  
  // 'else' blocks
  if(else_block) {
    if(else_ifs.size() == 0) {
#ifdef _DEBUG
      wcout << block_instructions.size() << L": " << L"label: id=" << next_label << endl;
#endif
      block_instructions.push_back(MakeInstruction(LBL, (int)next_label, 0));
      jump_table.insert(pair<long, size_t>(next_label, block_instructions.size() - 1));
    }
    // 'else' block
    EmitBlock(if_else->GetElseBlock(), block_instructions, jump_table);
  }
  
  // end label
#ifdef _DEBUG
  wcout << block_instructions.size() << L": " << L"label: id=" << end_label << endl;
#endif
	block_instructions.push_back(MakeInstruction(LBL, (int)end_label, 0));
	jump_table.insert(pair<long, size_t>(end_label, block_instructions.size() - 1));
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitWhile(While* if_while, vector<Instruction*> &block_instructions, 
                        unordered_map<long, size_t> &jump_table)
{
  /*
	const long top_label = label_id++;
	const long end_label = label_id++;
	
	
#ifdef _DEBUG
   wcout << block_instructions.size() << L": " << L"label: id=" << top_label << endl;
#endif
	block_instructions.push_back(MakeInstruction(LBL, (int)top_label, 0));
	jump_table.insert(pair<long, size_t>(top_label, block_instructions.size() - 1));
	
	// emit conditional expression
	EmitExpression(if_while->GetExpression(), block_instructions, jump_table);

	// emit; jump false
#ifdef _DEBUG
   wcout << block_instructions.size() << L": " << L"jump false: id=" << end_label << endl;
#endif
	 block_instructions.push_back(MakeInstruction(JMP, end_label, (int)0));
	
	// emit block
	EmitBlock(if_while->GetBlock(), block_instructions, jump_table);
	
#ifdef _DEBUG
  wcout << block_instructions.size() << L": " << L"jump: id=" << top_label << endl;
#endif
	block_instructions.push_back(MakeInstruction(JMP, top_label, (int)-1));
	
	// end label
#ifdef _DEBUG
  wcout << block_instructions.size() << L": " << L"label: id=" << end_label << endl;
#endif
	block_instructions.push_back(MakeInstruction(LBL, (int)end_label, 0));
	jump_table.insert(pair<long, size_t>(end_label, block_instructions.size() - 1));
  */
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitAssignment(Assignment* assignment, vector<Instruction*> &block_instructions, 
														 unordered_map<long, size_t> &jump_table)
{
	// emit expression
  EmitExpression(assignment->GetExpression(), block_instructions, jump_table);

  switch(assignment->GetAssignmentType()) {
  case TOKEN_ADD_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '+'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(ADD));
    break;

  case TOKEN_SUB_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '-'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(SUB));
    break;

  case TOKEN_MUL_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '*'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(MUL));
    break;

  case TOKEN_DIV_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '/'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(DIV));
    break;

  default:
    break;
  }

	// emit reference
  EmitReference(assignment->GetReference(), true, block_instructions, jump_table);
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitReference(Reference* reference, bool is_store, 
														vector<Instruction*> &block_instructions, 
														unordered_map<long, size_t> &jump_table)
{
  wstring name = reference->GetName();
  if(is_store) {
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"store: name='" << reference->GetName() 
					<< L"', id=" << reference->GetId() << endl;
#endif
    block_instructions.push_back(MakeInstruction(STOR_VAR, reference->GetId()));
  }
  else {
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"load: name='" << reference->GetName() 
					<< L"', id=" << reference->GetId() << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_VAR, reference->GetId()));
  }
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitExpression(Expression* expression, vector<Instruction*> &block_instructions, 
														 unordered_map<long, size_t> &jump_table)
{
  switch(expression->GetExpressionType()) {
  case REF_EXPR:
    EmitReference(static_cast<Reference*>(expression), false, block_instructions, jump_table);
    break;

  case BOOLEAN_LIT_EXPR:
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"load literal: type=boolean, value=" 
          << static_cast<BooleanLiteral*>(expression)->GetValue() << endl;
#endif
    if(static_cast<BooleanLiteral*>(expression)->GetValue()) {      
      block_instructions.push_back(MakeInstruction(LOAD_INT_LIT, (int)1));
    }
    else {
      block_instructions.push_back(MakeInstruction(LOAD_INT_LIT, (int)0));
    }
    break;
    
  case CHAR_LIT_EXPR:
    // TODO: implement
    break;
    
  case INT_LIT_EXPR:
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"load literal: type=integer, value=" 
          << static_cast<IntegerLiteral*>(expression)->GetValue() << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_INT_LIT, (int)static_cast<IntegerLiteral*>(expression)->GetValue()));
    break;
    
  case FLOAT_LIT_EXPR:
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"load literal: type=float, value=" 
          << static_cast<FloatLiteral*>(expression)->GetValue() << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_FLOAT_LIT, (FLOAT_T)static_cast<FloatLiteral*>(expression)->GetValue()));
    break;
    
  case AND_EXPR: {
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    const long next_label = NextEndId();
    const long end_label = NextEndId();
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"jump true: id=" << next_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(JMP, (int)next_label, 1));
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"load literal: type=integer, value=0" << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_INT_LIT, (int)0));
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"jump: id=" << end_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(JMP, (int)end_label, -1));

#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"label: id=" << next_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(LBL, (int)next_label, 0));
    jump_table.insert(pair<long, size_t>(next_label, block_instructions.size() - 1));
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);

#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"label: id=" << end_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(LBL, (int)end_label, 0));
    jump_table.insert(pair<long, size_t>(end_label, block_instructions.size() - 1));

#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '&'" << endl;
#endif
  }
    break;

  case OR_EXPR: {        
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    const long next_label = NextEndId();
    const long end_label = NextEndId();
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"jump false: id=" << next_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(JMP, (int)next_label, 0));
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"load literal: type=integer, value=0" << endl;
#endif
    block_instructions.push_back(MakeInstruction(LOAD_INT_LIT, (int)0));
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"jump: id=" << end_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(JMP, (int)end_label, -1));

#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"label: id=" << next_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(LBL, (int)next_label, 0));
    jump_table.insert(pair<long, size_t>(next_label, block_instructions.size() - 1));
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);

#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"label: id=" << end_label << endl;
#endif
    block_instructions.push_back(MakeInstruction(LBL, (int)end_label, 0));
    jump_table.insert(pair<long, size_t>(end_label, block_instructions.size() - 1));

#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '|'" << endl;
#endif
  }
    break;
    
  case EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '=='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(EQL));
  }
    break;

  case NEQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '!='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(NEQL));
  }
    break;

  case LES_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '<'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(LES));
  }
    break;

  case GTR_EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '>='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(GTR_EQL));
  }
    break;

  case LES_EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '<='" << endl;
#endif
    block_instructions.push_back(MakeInstruction(LES_EQL));
  }
    break;
    
  case GTR_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << block_instructions.size() << L": " << L"operator: '>'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(GTR));
  }
    break;

  case ADD_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '+'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(ADD));
  }
    break;
    
  case SUB_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '-'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(SUB));
  }
    break;

  case MUL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '*'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(MUL));
  }
    break;

  case DIV_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '/'" << endl;
#endif
    block_instructions.push_back(MakeInstruction(DIV));
  }
    break;
    
  case MOD_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions.size() << L": " << L"operator: '%'" << endl;
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
