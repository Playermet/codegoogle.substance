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
 * Emits an error
 ****************************/
void Emitter::ProcessError(ParseNode* node, const wstring &msg)
{
#ifdef _DEBUG
  wcout << L"\tError: " << node->GetFileName() << L":" << node->GetLineNumber() << L": " << msg << endl;
#endif

  const wstring &str_line_num = IntToString(node->GetLineNumber());
  errors.insert(pair<int, wstring>(node->GetLineNumber(), node->GetFileName() + L":" + str_line_num + L": " + msg));
}

/****************************
 * Emits an error
 ****************************/
void Emitter::ProcessError(const wstring &msg)
{
#ifdef _DEBUG
  wcout << L"\tError: " << msg << endl;
#endif

  errors.insert(pair<int, wstring>(0, msg));
}

/****************************
 * Check for errors detected
 * during the contextual
 * analysis process.
 ****************************/
bool Emitter::NoErrors()
{
  // check and process errors
  if(errors.size()) {
    map<int, wstring>::iterator error;
    for(error = errors.begin(); error != errors.end(); ++error) {
      wcerr << error->second << endl;
    }

    return false;
  }

  return true;
}

/****************************
 * Emit program instructions
 ****************************/
ExecutableProgram* Emitter::Emit()
{
#ifdef _DEBUG
  wcout << L"\n---------- Global Statements ---------" << endl;
#endif
  ExecutableProgram* executable_program = new ExecutableProgram;
  
  // emit global statements
  vector<Instruction*>* block_instructions = new vector<Instruction*>;
  unordered_map<long, size_t>* jump_table = new unordered_map<long, size_t>;
  set<size_t> leaders;
  EmitFunction(parsed_program->GetGlobal(), block_instructions, jump_table, leaders);
#ifdef _DEBUG
  wcout << block_instructions->size() << L": " << L"return" << endl;
#endif
  block_instructions->push_back(MakeInstruction(RTRN));    

  const int local_count = parsed_program->GetGlobalSymbolTable()->GetEntryCount();
  ExecutableFunction* global = new ExecutableFunction(L"#GLOBAL#", local_count, 0, block_instructions, jump_table, leaders, false);
  executable_program->SetGlobal(global);
  
  // emit functions
  vector<ParsedFunction*> functions = parsed_program->GetFunctions();
  for(size_t i = 0; i < functions.size(); i++) {
    ExecutableFunction* executable_function = EmitFunction(functions[i]);
    executable_program->AddFunction(executable_function);
  }

  // check for errors
  if(NoErrors()) {
    return executable_program;
  }
  return NULL;
}

ExecutableFunction* Emitter::EmitFunction(ParsedFunction* parsed_function)
{
#ifdef _DEBUG
  wcout << L"\n---------- Emitting Function: name='" << parsed_function->GetName() << L"' ---------" << endl;
#endif
  
  // create holders
  returns_value = -1;
  vector<Instruction*>* block_instructions = new vector<Instruction*>;
  unordered_map<long, size_t>* jump_table = new unordered_map<long, size_t>;
  set<size_t> leaders;
  
  vector<Expression*> parameters = parsed_function->GetParameters()->GetExpressions();
  for(size_t i = 0; i < parameters.size(); ++i) {
    Reference* reference = static_cast<Reference*>(parameters[i]);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"store: name='" << reference->GetName() 
					<< L"', id=" << reference->GetId() << endl;
#endif
    block_instructions->push_back(MakeInstruction(STOR_VAR, reference->GetId())); 
  }
  
  // emit function
  StatementList* statement_list = parsed_function->GetStatements();
  EmitFunction(statement_list, block_instructions, jump_table, leaders);
  if(block_instructions->back()->type != RTRN) {
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"return" << endl;
#endif
    block_instructions->push_back(MakeInstruction(RTRN));

    if(returns_value == 1) {
      ProcessError(statement_list->GetStatements().back(), L"Last statement must return a value");
    }
  }
  else {
    if(!static_cast<Return*>(statement_list->GetStatements().back())->GetExpression() && returns_value == 1) {
      ProcessError(statement_list->GetStatements().back(), L"Last statement must return a value");
    }
  }

#ifdef _DEBUG
  wcout << L"Leaders" << endl;
  set<size_t>::iterator iter = leaders.begin(); 
  for(;iter != leaders.end(); ++iter) {
    wcout << L"  " << *iter << endl;
  }
#endif
  
  const int local_count = parsed_function->GetSymbolTable()->GetEntryCount();
  return new ExecutableFunction(parsed_function->GetName(), local_count, (int)parameters.size(), 
    block_instructions, jump_table, leaders, returns_value > 0);
}

/****************************
 * Emit code for a function
 ****************************/
void Emitter::EmitFunction(StatementList* block_statements, vector<Instruction*>* block_instructions,
													 unordered_map<long, size_t>* jump_table, set<size_t> &leaders)
{
  EmitBlock(block_statements, block_instructions, jump_table);

  // create CFG
  leaders.insert(0);
  for(size_t i = 0; i < block_instructions->size(); ++i) {
    if(block_instructions->at(i)->type == LBL) {
      leaders.insert(i);      
    }
    else if(block_instructions->at(i)->type == JMP) {
      leaders.insert(i + 1);
    }
  }
}

/****************************
 * Emit code for a statement block
 ****************************/
void Emitter::EmitBlock(StatementList* block_statements, vector<Instruction*>* block_instructions,
												unordered_map<long, size_t>* jump_table)
{
  vector<Statement*> statements = block_statements->GetStatements();
  for(size_t i = 0; i < statements.size(); i++) {
    Statement* statement = statements[i];
    switch(statement->GetStatementType()) {
    case ASSIGNMENT_STATEMENT:
      EmitAssignment(static_cast<Assignment*>(statement), block_instructions, jump_table);
      break;
			
		case DECLARATION_STATEMENT:
			break;
			
    case RETURN_STATEMENT:
      if(static_cast<Return*>(statement)->GetExpression()) {
        EmitExpression(static_cast<Return*>(statement)->GetExpression(), block_instructions, jump_table);
        if(returns_value == 0) {
          ProcessError(statement, L"Not all statements return a value");
        }
        else {
          returns_value = 1;
        }
      }
      else {
        if(returns_value == 1) {
          ProcessError(statement, L"Not all statements return a value");
        }
        else {
          returns_value = 0;
        }
      }
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"return" << endl;
#endif
      block_instructions->push_back(MakeInstruction(RTRN));
			break;
      
		case FUNCTION_CALL_STATEMENT:
			EmitFunctionCall(static_cast<FunctionCall*>(statement), block_instructions, jump_table);
			break;
			
    case IF_ELSE_STATEMENT:
      EmitIfElse(static_cast<IfElse*>(statement), block_instructions, jump_table);
      break;
      
		case WHILE_STATEMENT:
			EmitWhile(static_cast<While*>(statement), block_instructions, jump_table);
      break;

    case DUMP_STATEMENT:
      EmitExpression(static_cast<Dump*>(statement)->GetExpression(), block_instructions, jump_table);
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"dump value" << endl;
#endif
      block_instructions->push_back(MakeInstruction(DUMP_VALUE));
      break;
      
    default:
      // TODO: report error
      break;
    }
  }
}

/****************************
 * Emit code for a method call
 ****************************/
void Emitter::EmitFunctionCall(FunctionCall* function_call, vector<Instruction*>* block_instructions, 
															 unordered_map<long, size_t>* jump_table)
{
	// emit calling parameters
  Reference* reference = function_call->GetReference();  
  vector<Expression*> parameters;
  if(reference->GetCallingParameters()) {
    // emit parameters
    parameters = reference->GetCallingParameters()->GetExpressions();	
    for(std::vector<Expression*>::reverse_iterator iter = parameters.rbegin(); iter != parameters.rend(); ++iter) {
      EmitExpression(*iter, block_instructions, jump_table);		
    }
  
    // emit function
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"function call: name='" << reference->GetName() << L"'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(CALL_FUNC, (INT_T)parameters.size(), function_call->ReturnsValue() ? 1 : 0, reference->GetName()));

    // array indices
    if(reference->GetIndices()) {
      vector<Expression*> indices = reference->GetIndices()->GetExpressions();	
      for(size_t i = 0; i < indices.size(); i++) {
        EmitExpression(indices[i], block_instructions, jump_table);		
      }
    }
  }
  // array indices
  else if(reference->GetIndices()) {
    vector<Expression*> indices = reference->GetIndices()->GetExpressions();	
    for(size_t i = 0; i < indices.size(); i++) {
      EmitExpression(indices[i], block_instructions, jump_table);		
    }
  }
  // class static function call
  else if(reference->GetReference()) {
    // emit parameters
    if(reference->GetReference()->GetCallingParameters()) {
      parameters = reference->GetReference()->GetCallingParameters()->GetExpressions();	
      for(std::vector<Expression*>::reverse_iterator iter = parameters.rbegin(); iter != parameters.rend(); ++iter) {
        EmitExpression(*iter, block_instructions, jump_table);		
      }
    }

    // array indices
    vector<Expression*> indices;
    if(reference->GetReference()->GetIndices()) {
      indices = reference->GetReference()->GetIndices()->GetExpressions();	
      for(size_t i = 0; i < indices.size(); i++) {
        EmitExpression(indices[i], block_instructions, jump_table);		
      }
    }

    // static call
    if(reference->GetId() < 0 && reference->GetReference()) {
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"function call: class='" << reference->GetName() << L"'" << ", function='" << reference->GetReference()->GetName() << L"'" << endl;
#endif
      INT_T num_params = 0;
      if(reference->GetReference()->GetReferenceType() == NEW_OBJ_TYPE) {
        num_params = (INT_T)indices.size();
      }
      else {
        num_params = (INT_T)parameters.size();
      }
      block_instructions->push_back(MakeInstruction(CALL_CLS_FUNC, num_params, function_call->ReturnsValue() ? 1 : 0, reference->GetName(), reference->GetReference()->GetName()));
    }
    // instance call
    else {
      ProcessError(reference, L"Unsupported function/method call");
    }
  }
  else {
    ProcessError(reference, L"Unsupported function/method call");
  }
}

/****************************
 * Emit 'if/else' code
 ****************************/
void Emitter::EmitIfElse(IfElse* if_else, vector<Instruction*>* block_instructions, 
                         unordered_map<long, size_t>* jump_table)
{
  const long end_label = NextEndId();
  long next_label = NextStartId();
  
  // emit test

  // emit expression
  if(if_else->GetExpression()->GetExpressionType() == FUNCTION_CALL_EXPR) {
    FunctionCall* function_call = static_cast<FunctionCall*>(if_else->GetExpression());
    function_call->ReturnsValue(true);
  }
	EmitExpression(if_else->GetExpression(), block_instructions, jump_table);

  // basic 'if' statement
  vector<IfElse*> else_ifs = if_else->GetElseIfs();
  StatementList* else_block = if_else->GetElseBlock();
  if(else_ifs.size() == 0 && !else_block) {
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"jump false: id=" << end_label << endl;
#endif
    block_instructions->push_back(MakeInstruction(JMP, (int)end_label, JMP_FALSE));
    // emit 'if' block
    EmitBlock(if_else->GetIfBlock(), block_instructions, jump_table);
  }
  else {
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"jump false: id=" << next_label << endl;
#endif
    block_instructions->push_back(MakeInstruction(JMP, (int)next_label, JMP_FALSE));
    // emit 'if' block
    EmitBlock(if_else->GetIfBlock(), block_instructions, jump_table);    
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"jump: id=" << end_label << endl;
#endif
    block_instructions->push_back(MakeInstruction(JMP, (int)end_label, -1));
  }
  
  // 'if-else' blocks
  if(else_ifs.size() > 0) {
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"label: id=" << next_label 
          << L", pos=" << block_instructions->size() << endl;
#endif
    block_instructions->push_back(MakeInstruction(LBL, (int)next_label, 0));
    jump_table->insert(pair<long, size_t>(next_label, block_instructions->size() - 1));
    
    for(size_t i = 0; i < else_ifs.size(); i++) {
      IfElse* else_if = else_ifs[i];
      // emit test
      EmitExpression(else_if->GetExpression(), block_instructions, jump_table);  
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"jump false: id=" << (next_label + 1) << endl;
#endif
      block_instructions->push_back(MakeInstruction(JMP, (int)next_label + 1, JMP_FALSE));
      
      // emit 'if' block
      EmitBlock(else_if->GetIfBlock(), block_instructions, jump_table);    
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"jump: id=" << end_label << endl;
#endif
      // jump to end
      block_instructions->push_back(MakeInstruction(JMP, (int)end_label, -1));
      
      next_label = NextStartId();
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"label: id=" << next_label 
            << L", pos=" << block_instructions->size() << endl;
#endif
      block_instructions->push_back(MakeInstruction(LBL, (int)next_label, 0));
      jump_table->insert(pair<long, size_t>(next_label, block_instructions->size() - 1));      
    }
  }
  
  // 'else' blocks
  if(else_block) {
    if(else_ifs.size() == 0) {
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"label: id=" << next_label 
            << L", pos=" << block_instructions->size() << endl;
#endif
      block_instructions->push_back(MakeInstruction(LBL, (int)next_label, 0));
      jump_table->insert(pair<long, size_t>(next_label, block_instructions->size() - 1));
    }
    // 'else' block
    EmitBlock(if_else->GetElseBlock(), block_instructions, jump_table);
  }
  
  // end label
#ifdef _DEBUG
  wcout << block_instructions->size() << L": " << L"label: id=" << end_label 
        << L", pos=" << block_instructions->size() << endl;
#endif
	block_instructions->push_back(MakeInstruction(LBL, (int)end_label, 0));
	jump_table->insert(pair<long, size_t>(end_label, block_instructions->size() - 1));
}

/****************************
 * TODO: doc
 ****************************/
void Emitter::EmitWhile(While* if_while, vector<Instruction*>* block_instructions, 
                        unordered_map<long, size_t>* jump_table)
{
  const long top_label = NextStartId();
  const long end_label = NextEndId();
	
  // top label
#ifdef _DEBUG
   wcout << block_instructions->size() << L": " << L"label: id=" << top_label 
         << L", pos=" << block_instructions->size() << endl;
#endif
	block_instructions->push_back(MakeInstruction(LBL, (int)top_label, 0));
	jump_table->insert(pair<long, size_t>(top_label, block_instructions->size() - 1));
	
	// emit test
	EmitExpression(if_while->GetExpression(), block_instructions, jump_table);

	// jump end
#ifdef _DEBUG
   wcout << block_instructions->size() << L": " << L"jump false: id=" << end_label << endl;
#endif
	 block_instructions->push_back(MakeInstruction(JMP, (int)end_label, JMP_FALSE));
	
	// emit block
	EmitBlock(if_while->GetBlock(), block_instructions, jump_table);
	
#ifdef _DEBUG
  wcout << block_instructions->size() << L": " << L"jump: id=" << top_label << endl;
#endif
	block_instructions->push_back(MakeInstruction(JMP, (int)top_label, JMP_UNCND));
	
	// end label
#ifdef _DEBUG
  wcout << block_instructions->size() << L": " << L"label: id=" << end_label 
        << L", pos=" << block_instructions->size() << endl;
#endif
	block_instructions->push_back(MakeInstruction(LBL, (int)end_label, 0));
	jump_table->insert(pair<long, size_t>(end_label, block_instructions->size() - 1));
}

/****************************
 * Emit assignment code
 ****************************/
void Emitter::EmitAssignment(Assignment* assignment, vector<Instruction*>* block_instructions, 
														 unordered_map<long, size_t>* jump_table)
{
	// emit expression
  if(assignment->GetExpression()->GetExpressionType() == FUNCTION_CALL_EXPR) {
    FunctionCall* function_call = static_cast<FunctionCall*>(assignment->GetExpression());
    function_call->ReturnsValue(true);
  }
  EmitExpression(assignment->GetExpression(), block_instructions, jump_table);
  
  switch(assignment->GetAssignmentType()) {
  case TOKEN_ADD_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '+'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(ADD));
    break;

  case TOKEN_SUB_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '-'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(SUB));
    break;

  case TOKEN_MUL_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '*'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(MUL));
    break;

  case TOKEN_DIV_EQL:
    EmitReference(assignment->GetReference(), false, block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '/'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(DIV));
    break;

  default:
    break;
  }

	// emit reference
  EmitReference(assignment->GetReference(), true, block_instructions, jump_table);
}

/****************************
 * Emit variable reference
 ****************************/
void Emitter::EmitReference(Reference* reference, bool is_store, vector<Instruction*>* block_instructions, 
														unordered_map<long, size_t>* jump_table)
{
  // emit array indices
  vector<Expression*> indices;
  if(reference->GetIndices()) {
    indices = reference->GetIndices()->GetExpressions();	
    for(size_t i = 0; i < indices.size(); i++) {
      EmitExpression(indices[i], block_instructions, jump_table);		
    }
  }

  if(is_store) {
    if(reference->GetIndices()) {
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"store array variable: name='" << reference->GetName() 
					  << L"', id=" << reference->GetId() << endl;
#endif
      block_instructions->push_back(MakeInstruction(STOR_ARY_VAR, reference->GetId(), (INT_T)indices.size()));
    }
    else {
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"store variable: name='" << reference->GetName() << L"', id=" << reference->GetId() << endl;
#endif
      block_instructions->push_back(MakeInstruction(STOR_VAR, reference->GetId()));
    }
  }
  else {
    if(reference->GetIndices()) {
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"load array variable: name='" << reference->GetName() << L"', id=" << reference->GetId() << endl;
#endif
      block_instructions->push_back(MakeInstruction(LOAD_ARY_VAR, reference->GetId(), (INT_T)indices.size()));
    }
    else {
#ifdef _DEBUG
      wcout << block_instructions->size() << L": " << L"load variable: name='" << reference->GetName() 
					  << L"', id=" << reference->GetId() << endl;
#endif
      block_instructions->push_back(MakeInstruction(LOAD_VAR, reference->GetId()));
    }
  }
}

/****************************
 * Emit expression
 ****************************/
void Emitter::EmitExpression(Expression* expression, vector<Instruction*>* block_instructions, 
														 unordered_map<long, size_t>* jump_table)
{
  switch(expression->GetExpressionType()) {
  case REF_EXPR:
    EmitReference(static_cast<Reference*>(expression), false, block_instructions, jump_table);
    break;

  case FUNCTION_CALL_EXPR:
    EmitFunctionCall(static_cast<FunctionCall*>(expression), block_instructions, jump_table);
    break;
    
  case BOOLEAN_LIT_EXPR:
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"load literal: type=boolean, value=" 
          << (static_cast<BooleanLiteral*>(expression)->GetValue() ? L"true" : L"false") << endl;
#endif     
    if(static_cast<BooleanLiteral*>(expression)->GetValue()) {      
      block_instructions->push_back(MakeInstruction(LOAD_TRUE_LIT));
    }
    else {
      block_instructions->push_back(MakeInstruction(LOAD_FALSE_LIT));
    }
    break;
    
  case CHAR_LIT_EXPR:
    // TODO: implement
    break;
    
  case INT_LIT_EXPR:
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"load literal: type=integer, value=" 
          << static_cast<IntegerLiteral*>(expression)->GetValue() << endl;
#endif
    block_instructions->push_back(MakeInstruction(LOAD_INT_LIT, (int)static_cast<IntegerLiteral*>(expression)->GetValue()));
    break;
    
  case FLOAT_LIT_EXPR:
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"load literal: type=float, value=" 
          << static_cast<FloatLiteral*>(expression)->GetValue() << endl;
#endif
    block_instructions->push_back(MakeInstruction(LOAD_FLOAT_LIT, (FLOAT_T)static_cast<FloatLiteral*>(expression)->GetValue()));
    break;
    
  case AND_EXPR: {
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    const long next_label = NextEndId();
    const long end_label = NextEndId();
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"jump true: id=" << next_label << endl;
#endif
    block_instructions->push_back(MakeInstruction(JMP, (int)next_label, JMP_TRUE));
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"load literal: type=boolean, value=false" << endl;
#endif
    block_instructions->push_back(MakeInstruction(LOAD_FALSE_LIT));
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"jump: id=" << end_label << endl;
#endif
    block_instructions->push_back(MakeInstruction(JMP, (int)end_label, JMP_UNCND));

#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"label: id=" << next_label 
          << L", pos=" << block_instructions->size() << endl;
#endif
    block_instructions->push_back(MakeInstruction(LBL, (int)next_label, JMP_FALSE));
    jump_table->insert(pair<long, size_t>(next_label, block_instructions->size() - 1));
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);

#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"label: id=" << end_label 
          << L", pos=" << block_instructions->size() << endl;
#endif
    block_instructions->push_back(MakeInstruction(LBL, (int)end_label, 0));
    jump_table->insert(pair<long, size_t>(end_label, block_instructions->size() - 1));

#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '&'" << endl;
#endif
  }
    break;

  case OR_EXPR: {        
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    const long next_label = NextEndId();
    const long end_label = NextEndId();
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"jump false: id=" << next_label << endl;
#endif
    block_instructions->push_back(MakeInstruction(JMP, (int)next_label, JMP_FALSE));
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"load literal: type=boolean, value=false" << endl;
#endif
    block_instructions->push_back(MakeInstruction(LOAD_FALSE_LIT));
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"jump: id=" << end_label << endl;
#endif
    block_instructions->push_back(MakeInstruction(JMP, (int)end_label, JMP_UNCND));

#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"label: id=" << next_label 
          << L", pos=" << block_instructions->size() << endl;
#endif
    block_instructions->push_back(MakeInstruction(LBL, (int)next_label, JMP_FALSE));
    jump_table->insert(pair<long, size_t>(next_label, block_instructions->size() - 1));
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);

#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"label: id=" << end_label 
          << L", pos=" << block_instructions->size() << endl;
#endif
    block_instructions->push_back(MakeInstruction(LBL, (int)end_label, 0));
    jump_table->insert(pair<long, size_t>(end_label, block_instructions->size() - 1));

#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '|'" << endl;
#endif
  }
    break;
    
  case EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '=='" << endl;
#endif
    block_instructions->push_back(MakeInstruction(EQL));
  }
    break;

  case NEQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '!='" << endl;
#endif
    block_instructions->push_back(MakeInstruction(NEQL));
  }
    break;

  case LES_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '<'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(LES));
  }
    break;

  case GTR_EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '>='" << endl;
#endif
    block_instructions->push_back(MakeInstruction(GTR_EQL));
  }
    break;

  case LES_EQL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '<='" << endl;
#endif
    block_instructions->push_back(MakeInstruction(LES_EQL));
  }
    break;
    
  case GTR_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << block_instructions->size() << L": " << L"operator: '>'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(GTR));
  }
    break;

  case ADD_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '+'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(ADD));
  }
    break;
    
  case SUB_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '-'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(SUB));
  }
    break;

  case MUL_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '*'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(MUL));
  }
    break;

  case DIV_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '/'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(DIV));
  }
    break;
    
  case MOD_EXPR: {    
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetRight(), block_instructions, jump_table);
    EmitExpression(static_cast<CalculatedExpression*>(expression)->GetLeft(), block_instructions, jump_table);
#ifdef _DEBUG
    wcout << block_instructions->size() << L": " << L"operator: '%'" << endl;
#endif
    block_instructions->push_back(MakeInstruction(MOD));
  }
    break;
    
  default:
    // TODO: error
    break;
  }
}

/****************************
 * Clear parse tree nodes
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
