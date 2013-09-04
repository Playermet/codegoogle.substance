/***************************************************************************
 * Language parser
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

#include "parser.h"

using namespace compiler;

/****************************
 * Loads parsing error codes
 ****************************/
void Parser::LoadErrorCodes()
{
  error_msgs[TOKEN_IF_ID] = L"Expected 'if'";
  error_msgs[TOKEN_IDENT] = L"Expected identifier";
  error_msgs[TOKEN_OPEN_PAREN] = L"Expected '('";
  error_msgs[TOKEN_CLOSED_PAREN] = L"Expected ')'";
  error_msgs[TOKEN_OPEN_BRACKET] = L"Expected '['";
  error_msgs[TOKEN_CLOSED_BRACKET] = L"Expected ']'";
  error_msgs[TOKEN_OPEN_BRACE] = L"Expected '{'";
  error_msgs[TOKEN_CLOSED_BRACE] = L"Expected '}'";
  error_msgs[TOKEN_COLON] = L"Expected ':'";
  error_msgs[TOKEN_COMMA] = L"Expected ','";
  error_msgs[TOKEN_ASSIGN] = L"Expected ':=' or '='";
  error_msgs[TOKEN_SEMI_COLON] = L"Expected ';'";
  error_msgs[TOKEN_ASSESSOR] = L"Expected '->'";
}

/****************************
 * Emits parsing error
 ****************************/
void Parser::ProcessError(ScannerTokenType type)
{
  wstring msg = error_msgs[type];
#ifdef _DEBUG
  wcout << L"\tError: " << GetFileName() << L":" << GetLineNumber() << L": "
        << msg << endl;
#endif

  const wstring &str_line_num = ToString(GetLineNumber());
  errors.insert(pair<int, wstring>(GetLineNumber(), GetFileName() + L":" + str_line_num + L": " + msg));
}

/****************************
 * Emits parsing error
 ****************************/
void Parser::ProcessError(const wstring &msg)
{
#ifdef _DEBUG
  wcout << L"\tError: " << GetFileName() << L":" << GetLineNumber() << L": " << msg << endl;
#endif

  const wstring &str_line_num = ToString(GetLineNumber());
  errors.insert(pair<int, wstring>(GetLineNumber(), GetFileName() + L":" + str_line_num + L": " + msg));
}

/****************************
 * Emits parsing error
 ****************************/
void Parser::ProcessError(const wstring &msg, ScannerTokenType sync)
{
#ifdef _DEBUG
  wcout << L"\tError: " << GetFileName() << L":" << GetLineNumber() << L": "
        << msg << endl;
#endif

  const wstring &str_line_num = ToString(GetLineNumber());
  errors.insert(pair<int, wstring>(GetLineNumber(), GetFileName() + L":" + str_line_num + L": " + msg));
  ScannerTokenType token = GetToken();
  while(token != sync && token != TOKEN_END_OF_STREAM) {
    NextToken();
    token = GetToken();
  }
}

/****************************
 * Emits parsing error
 ****************************/
void Parser::ProcessError(const wstring &msg, ParseNode* node)
{
#ifdef _DEBUG
  wcout << L"\tError: " << node->GetFileName() << L":" << node->GetLineNumber()
        << L": " << msg << endl;
#endif

  const wstring &str_line_num = ToString(node->GetLineNumber());
  errors.insert(pair<int, wstring>(node->GetLineNumber(), node->GetFileName() +
																	 L":" + str_line_num + L": " + msg));
}

/****************************
 * Checks for parsing errors
 * returns false if errors
 ****************************/
bool Parser::NoErrors()
{
  // check and process errors
  if(errors.size()) {
    for(size_t i = 0; i < errors.size(); i++) {
      wcerr << errors[i] << endl;
    }
    // clean up
    return false;
  }

  return true;
}

/****************************
 * Starts the parsing process.
 ****************************/
ParsedProgram* Parser::Parse()
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

#ifdef _DEBUG
  wcout << L"\n========== Scanning/Parsing =========" << endl;
#endif

  NextToken();
  
  program = new ParsedProgram;
	symbol_table = new SymbolTable;
	SymbolTable* prev_symbol_table;
	
  StatementList* statements = TreeFactory::Instance()->MakeStatementList(file_name, line_num);
	
	// scope for global statements
  symbol_table->NewScope();
	
  while(!Match(TOKEN_END_OF_STREAM)) {
    // parse class
    if(Match(TOKEN_CLASS_ID)) {
      // new scope for function level statements
			prev_symbol_table = symbol_table;			
			symbol_table = new SymbolTable;			
      ParsedClass* klass = ParseClass(0);
      if(klass) {
        klass->SetSymbolTable(symbol_table);
        symbol_table = prev_symbol_table;			
      }
      else {
        DeleteProgram();
        return NULL;
      }
      // add function
			if(!program->AddClass(klass)) {
				ProcessError(L"Class with the same name already exists", klass);
			}
    }
		// parse function
    else if(Match(TOKEN_FUNC_ID)) {
			// new scope for function level statements
			prev_symbol_table = symbol_table;			
			symbol_table = new SymbolTable;			
      ParsedFunction* function = ParseFunction(false, 0);
      if(function) {
        function->SetSymbolTable(symbol_table);
        symbol_table = prev_symbol_table;			
      }
      else {
        DeleteProgram();
        return NULL;
      }
      // add function
			if(!program->AddFunction(function)) {
				ProcessError(L"Function with the same name already exists", function);
			}
    }
		// parse global statement
    else {
      Statement* statement = ParseStatement(0);
      if(!statement) {
        DeleteProgram();
        return NULL;
      }
      statements->AddStatement(statement);
    }
  }
  program->SetGlobal(statements);
  
	// clean up symbol table
  symbol_table->PreviousScope();
	program->SetGlobalSymbolTable(symbol_table);
	symbol_table = NULL;
	
  if(NoErrors()) {
    return program;
  }
  
  return NULL;
}

/****************************
 * Parses a class
 ****************************/
ParsedClass* Parser::ParseClass(int depth)
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

#ifdef _DEBUG
	Show(L"Class", depth);
#endif
  
  NextToken();

  if(!Match(TOKEN_IDENT)) {
		ProcessError(TOKEN_IDENT);
		return NULL;
	}
  ParsedClass* klass = TreeFactory::Instance()->MakeClass(file_name, line_num, scanner->GetToken()->GetIdentifier());
	NextToken();
  
  if(!Match(TOKEN_OPEN_BRACE)) {
    ProcessError(TOKEN_OPEN_BRACE);
    return NULL;
  }
  NextToken();

  Declarations* declarations = TreeFactory::Instance()->MakeDeclarations(file_name, line_num);
  while(!Match(TOKEN_END_OF_STREAM) && (Match(TOKEN_VAR_ID) || Match(TOKEN_FUNC_ID) || Match(TOKEN_NEW_ID))) {
    // variables
    if(Match(TOKEN_VAR_ID)) {
      NextToken();
      while(!Match(TOKEN_END_OF_STREAM) && !Match(TOKEN_SEMI_COLON)) {
        Declaration* declaration = static_cast<Declaration*>(ParseDeclaration(depth + 1));
        if(!declaration) {
          return NULL;
        }
        declarations->AddDeclaration(declaration);
        if(!Match(TOKEN_SEMI_COLON) && !Match(TOKEN_COMMA)) {
          ProcessError(L"Expected ',' or ';'", declarations);
          return NULL;
        } 
        
        if(Match(TOKEN_COMMA)) {
          NextToken();
        } 
      }
      
      if(!Match(TOKEN_SEMI_COLON)) {
        ProcessError(TOKEN_SEMI_COLON);
        return NULL;
      }
      NextToken();
    }
    // functions
    else if(Match(TOKEN_FUNC_ID) || Match(TOKEN_NEW_ID)) {
      const bool is_new = Match(TOKEN_NEW_ID);
      ParsedFunction* function = ParseFunction(is_new, depth + 1);
      if(!function) {
        return NULL;
      }
      klass->AddFunction(function);
    }
  }
  klass->SetDeclarations(declarations);
  
  if(!Match(TOKEN_CLOSED_BRACE)) {
    ProcessError(TOKEN_CLOSED_BRACE);
    return NULL;
  }
  NextToken();
  
  return klass;
}

/****************************
 * Parses a function
 ****************************/
ParsedFunction* Parser::ParseFunction(bool is_new, int depth)
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

#ifdef _DEBUG
	Show(L"Function", depth);
#endif
	
	NextToken();
	
  wstring name;
  if(is_new) {
    name = L"New";
  }
  else {
    if(!Match(TOKEN_COLON)) {
		  ProcessError(TOKEN_COLON);
		  return NULL;
	  }
    NextToken();

    if(!Match(TOKEN_IDENT)) {
		  ProcessError(TOKEN_IDENT);
		  return NULL;
	  }
    name = scanner->GetToken()->GetIdentifier();
	  NextToken();
  }
    
  symbol_table->NewScope();

  ExpressionList* parameters = ParseDeclarationParameters(depth + 1);
  StatementList* statements = ParseBlock(false, 0);
  if(!parameters || !statements) {
    return NULL;
  }

  symbol_table->PreviousScope();

  return TreeFactory::Instance()->MakeFunction(file_name, line_num, name, parameters, statements, is_new);
}

/****************************
 * Parses a parameter list
 ****************************/
ExpressionList* Parser::ParseDeclarationParameters(int depth)
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

  ExpressionList* parameters = TreeFactory::Instance()->MakeExpressionList(file_name, line_num);
  if(!Match(TOKEN_OPEN_PAREN)) {
		ProcessError(TOKEN_OPEN_PAREN);
		return NULL;
	}
  NextToken();

  while(!Match(TOKEN_END_OF_STREAM) && Match(TOKEN_IDENT)) {
    const wstring identifier = scanner->GetToken()->GetIdentifier();
    if(symbol_table->HasEntry(identifier)) {
		  ProcessError(L"Variable already declared in this scope");
		  return NULL;
	  }	
    symbol_table->AddEntry(identifier);
    Reference* parameter = ParseReference(identifier, depth + 1);
    if(!parameter) {
      return NULL;
    }
    parameters->AddExpression(parameter);

    if(Match(TOKEN_COMMA)) {
      NextToken();
      if(Match(TOKEN_CLOSED_PAREN)) {
        ProcessError(TOKEN_IDENT);
      }
    }
  }

  if(!Match(TOKEN_CLOSED_PAREN)) {
		ProcessError(TOKEN_CLOSED_PAREN);
		return NULL;
	}
  NextToken();

  return parameters;
}

/****************************
 * Parses a statement block
 ****************************/
StatementList* Parser::ParseBlock(bool new_scope, int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

	if(new_scope) {
		symbol_table->NewScope();
	}

  StatementList* block = TreeFactory::Instance()->MakeStatementList(file_name, line_num);  
  if(!Match(TOKEN_OPEN_BRACE)) {
    ProcessError(TOKEN_OPEN_BRACE);
    return NULL;
  }
  NextToken();

  while(!Match(TOKEN_CLOSED_BRACE) && !Match(TOKEN_END_OF_STREAM)) {
		Statement* statement = ParseStatement(depth + 1);
		if(statement) {
			block->AddStatement(statement);
		}
		else {
			return NULL;
		}
  }
  
  if(!Match(TOKEN_CLOSED_BRACE)) {
    ProcessError(TOKEN_CLOSED_BRACE);
    return NULL;
  }
  NextToken();
	
	if(new_scope) {
		symbol_table->PreviousScope();
	}

  return block;
}

/****************************
 * Parses a statement.
 ****************************/
Statement* Parser::ParseStatement(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

  Statement* statement;
  switch(scanner->GetToken()->GetType()) {
    // reference type
  case TOKEN_IDENT: {
		Reference* reference = ParseReference(scanner->GetToken()->GetIdentifier(), depth + 1);
		if(!reference) {
			return NULL;
		}		
		// function call
		if(reference->IsMethodReference()) {
			statement = TreeFactory::Instance()->MakeFunctionCall(file_name, line_num, reference);
		}
		// assignment
		else {
			ScannerTokenType type = TOKEN_UNKNOWN;
			switch(GetToken()) {
			case TOKEN_ASSIGN:
			case TOKEN_ADD_EQL:
			case TOKEN_SUB_EQL:
			case TOKEN_MUL_EQL:
			case TOKEN_DIV_EQL:
				type = GetToken();
				break;				
			default:
				ProcessError(TOKEN_ASSIGN);
				return NULL;
			} 
			NextToken(); 			
			statement = ParseAssignment(reference, type, depth + 1);
		}
	}
    break;
		
		// return
  case TOKEN_RETURN_ID: {
    NextToken();
    Expression* expression = NULL;
    if(!Match(TOKEN_SEMI_COLON)) {
      expression = ParseExpression(depth + 1);
    }
    statement = TreeFactory::Instance()->MakeReturnStatement(file_name, line_num, expression);
  }
    break;
		
		// if
  case TOKEN_IF_ID:
    statement = ParseIfElse(depth + 1);
    break;
		
		// while
  case TOKEN_WHILE_ID:
    statement = ParseWhile(depth + 1);
    break;

		// var
	case TOKEN_VAR_ID: {
    Declarations* declarations = TreeFactory::Instance()->MakeDeclarations(file_name, line_num);
    NextToken();
    while(!Match(TOKEN_END_OF_STREAM) && !Match(TOKEN_SEMI_COLON)) {
      Statement* declaration = ParseDeclaration(depth + 1);
      if(!declaration) {
        return NULL;
      }
      // declarations->AddStatement(declaration);
      if(!Match(TOKEN_SEMI_COLON) && !Match(TOKEN_COMMA)) {
        ProcessError(L"Expected ',' or ';'", declarations);
        return NULL;
      } 
        
      if(Match(TOKEN_COMMA)) {
        NextToken();
      } 
    } 
    statement = declarations;
  }
		break;
		    
    // value dump
  case TOKEN_DUMP_ID: {
#ifdef _DEBUG
    Show(L"Dump", depth);
#endif
    NextToken();
    statement = TreeFactory::Instance()->MakeDumpStatement(file_name, line_num, ParseExpression(depth + 1));
  }
    break;

  default:
    ProcessError(L"Invalid statement");
		return NULL;
  }
  
  // statement end
  if(!Match(TOKEN_SEMI_COLON)) {
    ProcessError(TOKEN_SEMI_COLON);
    return NULL;
  }
  NextToken();
  
  return statement;
}

/****************************
 * Parses a declaration
 ****************************/
Statement* Parser::ParseDeclaration(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

#ifdef _DEBUG
	Show(L"Var", depth);
#endif
	
	// variable assignment
	if(Match(TOKEN_IDENT) && Match(TOKEN_ASSIGN, SECOND_INDEX)) {
		Reference* reference = ParseReference(scanner->GetToken()->GetIdentifier(), depth + 1);
		if(!reference) {
			return NULL;
		}
		else if(!reference->IsMethodReference()) {
			NextToken();
			return ParseAssignment(reference, TOKEN_ASSIGN, depth + 1);
		}
	}
	
	if(!Match(TOKEN_IDENT)) {
		ProcessError(TOKEN_IDENT);
		return NULL;
	}

  // TODO: get fully qualified name
	const wstring identifier = scanner->GetToken()->GetIdentifier();
	NextToken(); 
		
	if(symbol_table->HasEntry(identifier)) {
		ProcessError(L"Variable already declared in this scope");
		return NULL;
	}	
	symbol_table->AddEntry(identifier); 
	
	return TreeFactory::Instance()->MakeDeclarationStatement(file_name, line_num, identifier);
}

/****************************
 * Parses an 'if/else' 
 * statement.
 ****************************/
Statement* Parser::ParseIfElse(int depth)
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

#ifdef _DEBUG
	Show(L"If/Else", depth);
#endif

  // conditional expression
  if(!Match(TOKEN_IF_ID)) {
    ProcessError(TOKEN_IF_ID);
    return NULL;
  }
  NextToken();

  if(!Match(TOKEN_OPEN_PAREN)) {
    ProcessError(TOKEN_OPEN_PAREN);
    return NULL;
  }
  NextToken();

	Expression* expression = ParseExpression(depth + 1);
	if(!Match(TOKEN_CLOSED_PAREN)) {
    ProcessError(TOKEN_CLOSED_PAREN);
    return NULL;
  }
  NextToken();

  // statement block
	StatementList* if_block = ParseBlock(true, depth + 1);
	if(expression && if_block) {
		IfElse* if_else = TreeFactory::Instance()->MakeIfElseStatement(file_name, line_num, expression, if_block);
    // find 'else/if' blocks
    bool found_else = false;
    while(!Match(TOKEN_END_OF_STREAM) && Match(TOKEN_ELSE_ID) && !found_else) {
      NextToken();      
      // 'else if' part
      if(Match(TOKEN_IF_ID)) {        
        NextToken();
        
        if(!Match(TOKEN_OPEN_PAREN)) {
          ProcessError(TOKEN_OPEN_PAREN);
          return NULL;
        }
        NextToken();
        
        Expression* else_if_expression = ParseExpression(depth + 1);
        if(!Match(TOKEN_CLOSED_PAREN)) {
          ProcessError(TOKEN_CLOSED_PAREN);
          return NULL;
        }
        NextToken();
        
        // statement block
        StatementList* else_if_block = ParseBlock(true, depth + 1);
        if(else_if_expression && else_if_block) {
          if_else->AddElseIf(TreeFactory::Instance()->MakeIfElseStatement(file_name, line_num, 
                                                                          else_if_expression, 
                                                                          else_if_block));
        }
        else {
          return NULL;
        }
      }
      // 'else' part
      else {
        StatementList* else_block = ParseBlock(true, depth + 1);
        if(!else_block) {
          return NULL;
        }
        if_else->SetElseBlock(else_block);
        found_else = true;
      }
    }
    
    return if_else;
	}
	
	return NULL;
}

/****************************
 * Parses an 'if' statement.
 ****************************/
Statement* Parser::ParseWhile(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
	Show(L"While", depth);
#endif

	NextToken();

  if(!Match(TOKEN_OPEN_PAREN)) {
    ProcessError(TOKEN_OPEN_PAREN);
    return NULL;
  }
  NextToken();

	Expression* expression = ParseExpression(depth + 1);

	if(!Match(TOKEN_CLOSED_PAREN)) {
    ProcessError(TOKEN_CLOSED_PAREN);
    return NULL;
  }
  NextToken();

	StatementList* block = ParseBlock(true, depth + 1);
	
	if(expression && block) {
		return TreeFactory::Instance()->MakeWhileStatement(file_name, line_num, expression, block);
	}
	
	return NULL;
}

/****************************
 * Parses an assignment 
 * statement.
 ****************************/
Statement* Parser::ParseAssignment(Reference* reference, ScannerTokenType type, int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Assignment", depth);
#endif
  
  Expression* expression = ParseExpression(depth + 1);
  if(reference && expression) {
    return TreeFactory::Instance()->MakeAssignmentStatement(file_name, line_num, type, reference, expression);
  }
  
  return NULL;
}

/****************************
 * Parses an expression.
 ****************************/
Expression* Parser::ParseExpression(int depth)
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
  
#ifdef _DEBUG
  Show(L"Expression", depth);
#endif
  
  Expression* expression = NULL;

  // hash allocation
  if(Match(TOKEN_OPEN_BRACE) && Match(TOKEN_CLOSED_BRACE, SECOND_INDEX)) {
#ifdef _DEBUG
  Show(L"New hash", depth + 1);
#endif
    NextToken();
    expression = TreeFactory::Instance()->MakeHashAllocation(file_name, line_num);
  } 
  // list allocation
  else if(Match(TOKEN_OPEN_BRACKET) && Match(TOKEN_CLOSED_BRACKET, SECOND_INDEX)) {
#ifdef _DEBUG
  Show(L"New list", depth + 1);
#endif
    NextToken(); NextToken();
    expression = TreeFactory::Instance()->MakeListAllocation(file_name, line_num);
  } 
  else {
	  expression = ParseLogic(depth + 1);
    // check for function call
    if(expression && expression->GetExpressionType() == REF_EXPR) {
      Reference* reference = static_cast<Reference*>(expression);
      // TODO: move into function?
      while(reference->GetReference()) {
        reference = reference->GetReference();
      }
      switch(reference->GetReferenceType()) {
      case NEW_LIST_TYPE:
      case NEW_HASH_TYPE:
      case NEW_OBJ_TYPE:
        expression = TreeFactory::Instance()->MakeFunctionCall(file_name, line_num, static_cast<Reference*>(expression));
        break;

      default:
        if(static_cast<Reference*>(expression)->IsMethodReference()) {
          expression = TreeFactory::Instance()->MakeFunctionCall(file_name, line_num, static_cast<Reference*>(expression));
        }
        break;
      }
    }  
  }

  return expression;
}

/****************************
 * Parses a logical expression.
 * This method delegates support
 * for other types of expressions.
 ****************************/
Expression* Parser::ParseLogic(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

#ifdef _DEBUG
  Show(L"Boolean logic", depth);
#endif

  Expression* left = ParseMathLogic(depth + 1);

  CalculatedExpression* expression = NULL;
  while(!Match(TOKEN_END_OF_STREAM) && ((Match(TOKEN_AND) || Match(TOKEN_OR)))) {
    if(expression) {
      left = expression;
    }

    switch(GetToken()) {
    case TOKEN_AND:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, AND_EXPR);
      break;
    case TOKEN_OR:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, OR_EXPR);
      break;

    default:
      break;
    }
    NextToken();

    Expression* right = ParseLogic(depth + 1);
    if(expression) {
      expression->SetLeft(right);
      expression->SetRight(left);
    }
  }

  if(expression) {
    return expression;
  }

  // pass-thru
  return left;
}

/****************************
 * Parses a mathematical expression.
 * This method delegates support
 * for other types of expressions.
 ****************************/
Expression* Parser::ParseMathLogic(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Boolean math", depth);
#endif

  Expression* left = ParseTerm(depth + 1);

  if(Match(TOKEN_LES) || Match(TOKEN_GTR) ||
     Match(TOKEN_LEQL) || Match(TOKEN_GEQL) ||
     Match(TOKEN_EQL) || Match(TOKEN_NEQL)) {
    CalculatedExpression* expression = NULL;
    switch(GetToken()) {
    case TOKEN_LES:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, LES_EXPR);
      break;
    case TOKEN_GTR:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, GTR_EXPR);
      break;
    case TOKEN_LEQL:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, LES_EQL_EXPR);
      break;
    case TOKEN_GEQL:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, GTR_EQL_EXPR);
      break;
    case TOKEN_EQL:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, EQL_EXPR);
      break;
    case TOKEN_NEQL:
      expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, NEQL_EXPR);
      break;

    default:
      break;
    }
    NextToken();

    if(expression) {
      Expression* right = ParseTerm(depth + 1);
      expression->SetLeft(left);
      expression->SetRight(right);
    }

    return expression;
  }

  // pass-thru
  return left;
}

/****************************
 * Parses a mathematical term.
 * This method delegates support
 * for other types of expressions.
 ****************************/
Expression* Parser::ParseTerm(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Term", depth);
#endif

  Expression* left = ParseFactor(depth + 1);
  if(!left) {
    return NULL;
  }

  if(!Match(TOKEN_ADD) && !Match(TOKEN_SUB)) {
    return left;
  }

  CalculatedExpression* expression = NULL;
  while(!Match(TOKEN_END_OF_STREAM) && ((Match(TOKEN_ADD) || Match(TOKEN_SUB)))) {
    if(expression) {
      CalculatedExpression* right;
      if(Match(TOKEN_ADD)) {
        right = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, ADD_EXPR);
      } else {
        right = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, SUB_EXPR);
      }
      NextToken();

      Expression* temp = ParseFactor(depth + 1);

      right->SetRight(temp);
      right->SetLeft(expression);
      expression = right;
    }
    // first time in loop
    else {
      if(Match(TOKEN_ADD)) {
        expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, ADD_EXPR);
      } else {
        expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, SUB_EXPR);
      }
      NextToken();

      Expression* temp = ParseFactor(depth + 1);

      if(expression) {
        expression->SetRight(temp);
        expression->SetLeft(left);
      }
    }
  }

  return expression;
}

/****************************
 * Parses a mathematical factor.
 * This method delegates support
 * for other types of expressions.
 ****************************/
Expression* Parser::ParseFactor(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Factor", depth);
#endif

  Expression* left = ParseSimpleExpression(depth + 1);
  if(!Match(TOKEN_MUL) && !Match(TOKEN_DIV) && !Match(TOKEN_MOD)) {
    return left;
  }

  CalculatedExpression* expression = NULL;
  while(!Match(TOKEN_END_OF_STREAM) && (Match(TOKEN_MUL) || Match(TOKEN_DIV) || Match(TOKEN_MOD))) {
    if(expression) {
      CalculatedExpression* right;
      if(Match(TOKEN_MUL)) {
        right = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, MUL_EXPR);
      } else if(Match(TOKEN_MOD)) {
        right = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, MOD_EXPR);
      } else {
        right = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, DIV_EXPR);
      }
      NextToken();

      Expression* temp = ParseSimpleExpression(depth + 1);
      right->SetRight(temp);
      right->SetLeft(expression);
      expression = right;
    }
    // first time in loop
    else {
      if(Match(TOKEN_MUL)) {
        expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, MUL_EXPR);
      } else if(Match(TOKEN_MOD)) {
        expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, MOD_EXPR);
      } else {
        expression = TreeFactory::Instance()->MakeCalculatedExpression(file_name, line_num, DIV_EXPR);
      }
      NextToken();

      Expression* temp = ParseSimpleExpression(depth + 1);
      if(expression) {
        expression->SetRight(temp);
        expression->SetLeft(left);
      }
    }
  }

  return expression;
}

/****************************
 * Parses array indices.
 ****************************/
ExpressionList* Parser::ParseIndices(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
  ExpressionList* expressions = NULL;
  if(Match(TOKEN_OPEN_BRACKET)) {
    expressions = TreeFactory::Instance()->MakeExpressionList(file_name, line_num);
    NextToken();

    while(!Match(TOKEN_END_OF_STREAM) && !Match(TOKEN_CLOSED_BRACKET)) {
      // expression
      Expression* expression = ParseExpression(depth + 1);
      if(!expression) {
        return NULL;
      }
      expressions->AddExpression(expression);
      
      if(Match(TOKEN_COMMA)) {
        NextToken();
      }
      else if(!Match(TOKEN_CLOSED_BRACKET)) {
        ProcessError(L"Expected comma or semi-colon");
        NextToken();
      }
    }

    if(!Match(TOKEN_CLOSED_BRACKET)) {
      ProcessError(TOKEN_CLOSED_BRACKET);
    }
    NextToken();
  }

  return expressions;
}

/****************************
 * Parses a simple expression.
 ****************************/
Expression* Parser::ParseSimpleExpression(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Simple expression", depth);
#endif
  Expression* expression = NULL;

  if(Match(TOKEN_IDENT)) {
    const wstring &identifier = scanner->GetToken()->GetIdentifier();
    expression = ParseReference(identifier, depth + 1);
  }
  else if(Match(TOKEN_SELF_ID)) {
    NextToken();
    expression = ParseSelf(depth + 1);
  }
  else if(Match(TOKEN_SUB)) {
    NextToken();

    switch(GetToken()) {
    case TOKEN_INT_LIT:
      expression = TreeFactory::Instance()->MakeIntegerLiteral(file_name, line_num, 
																															 -scanner->GetToken()->GetIntLit());
      NextToken();
      break;

    case TOKEN_FLOAT_LIT:
      expression = TreeFactory::Instance()->MakeFloatLiteral(file_name, line_num, 
																														 -scanner->GetToken()->GetFloatLit());
      NextToken();
      break;

    default:
      ProcessError(L"Expected expression");
      NextToken();
      break;
    }
  }
  else if(Match(TOKEN_OPEN_PAREN)) {
    NextToken();
    expression = ParseLogic(depth + 1);
    if(!Match(TOKEN_CLOSED_PAREN)) {
      ProcessError(TOKEN_CLOSED_PAREN);
    }
    NextToken();
  }
  else {
    switch(GetToken()) {
    case TOKEN_TRUE_LIT:
      expression = TreeFactory::Instance()->MakeBooleanLiteral(file_name, line_num, true);
      NextToken();
      break;

    case TOKEN_FALSE_LIT:
      expression = TreeFactory::Instance()->MakeBooleanLiteral(file_name, line_num, false);
      NextToken();
      break;

    case TOKEN_CHAR_LIT:
      expression = TreeFactory::Instance()->MakeCharacterLiteral(file_name, line_num, 
																																 scanner->GetToken()->GetCharLit());
      NextToken();
      break;

    case TOKEN_INT_LIT:
      expression = TreeFactory::Instance()->MakeIntegerLiteral(file_name, line_num, 
																															 scanner->GetToken()->GetIntLit());
      NextToken();
      break;

    case TOKEN_FLOAT_LIT:
      expression = TreeFactory::Instance()->MakeFloatLiteral(file_name, line_num, 
																														 scanner->GetToken()->GetFloatLit());
      NextToken();
      break;

    case TOKEN_CHAR_STRING_LIT: {
      const wstring &identifier = scanner->GetToken()->GetIdentifier();
      expression = TreeFactory::Instance()->MakeCharacterString(file_name, line_num, identifier);
      NextToken();
    }
      break;

    default:
      ProcessError(L"Expected expression");
      NextToken();
      break;
    }
  }

  // subsequent instance references
  if(Match(TOKEN_ASSESSOR)) {
    if(expression && expression->GetExpressionType() == REF_EXPR) {
      ParseReference(static_cast<Reference*>(expression), depth + 1);
    }
    else {
      ProcessError(L"Expected reference");
      NextToken();
    }
  }

  return expression;
}

/****************************
 * Parses a instance reference.
 ****************************/
Reference* Parser::ParseSelf(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Self", depth);
#endif

	// self reference
	Reference* self_ref = TreeFactory::Instance()->MakeSelf(file_name, line_num);

  // subsequent instance references
  if(Match(TOKEN_ASSESSOR)) {
    ParseReference(self_ref, depth + 1);
  }

  return self_ref;
}

/****************************
 * Parses a instance reference.
 ****************************/
Reference* Parser::ParseReference(const wstring &identifier, int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Reference: name=" + identifier, depth);
#endif

	NextToken();
	
	// scalar or array reference
	bool is_function_call = false;
	Reference* reference;
	if(!Match(TOKEN_OPEN_PAREN)) {
    // check to see if the reference is nested
    if(!Match(TOKEN_ASSESSOR)) {
      // add reference to table if it doesn't exist
      if(!symbol_table->HasEntry(identifier)) {
        symbol_table->AddEntry(identifier);
      }	
      reference = TreeFactory::Instance()->MakeReference(file_name, line_num, identifier, symbol_table->GetEntry(identifier));
    }
    else {
      reference = TreeFactory::Instance()->MakeReference(file_name, line_num, identifier, -1);
    }
	}
	else {
		reference = TreeFactory::Instance()->MakeReference(file_name, line_num, identifier);
		is_function_call = true;
	}
	
	// array reference
  if(Match(TOKEN_OPEN_BRACKET)) {
    reference->SetIndices(ParseIndices(depth + 1));
  }
	
	// function call
	if(is_function_call) {
    reference->SetCallingParameters(ParseCallingParameters(depth + 1));
  }
	
  // subsequent instance references
  if(Match(TOKEN_ASSESSOR)) {
    ParseReference(reference, depth + 1);
  }

  return reference;
}

/****************************
 * Parses an instance reference.
 ****************************/
void Parser::ParseReference(Reference* reference, int depth)
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
  
  NextToken();
  
  Reference* nested_reference = NULL;
  if(Match(TOKEN_IDENT)) {
    // identifier
    const wstring &identifier = scanner->GetToken()->GetIdentifier();
    NextToken();
    
#ifdef _DEBUG
    Show(L"Nested reference: name=" + identifier, depth);
#endif
    nested_reference = TreeFactory::Instance()->MakeReference(file_name, line_num, identifier);
  }
  else if(Match(TOKEN_NEW_ID)) {
    NextToken();
#ifdef _DEBUG
    Show(L"New", depth);
#endif
    nested_reference = TreeFactory::Instance()->MakeNew(file_name, line_num);
  }
  else {
    ProcessError(L"Expected identifier or 'New'", reference);
    return;
  }
  reference->SetReference(nested_reference);
  
  // array reference
  if(Match(TOKEN_OPEN_BRACKET)) {
    nested_reference->SetIndices(ParseIndices(depth + 1));
  }
	
	// function call
  if(Match(TOKEN_OPEN_PAREN)) {
    nested_reference->SetCallingParameters(ParseCallingParameters(depth + 1));
  }

  // subsequent instance references
  if(Match(TOKEN_ASSESSOR)) {
    ParseReference(nested_reference, depth + 1);
  }
}

/****************************
 * Parses a function call
 ****************************/
ExpressionList* Parser::ParseCallingParameters(int depth)
{
  const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

  ExpressionList* parameters = TreeFactory::Instance()->MakeExpressionList(file_name, line_num);
  if(!Match(TOKEN_OPEN_PAREN)) {
		ProcessError(TOKEN_OPEN_PAREN);
		return NULL;
	}
  NextToken();

  while(!Match(TOKEN_END_OF_STREAM) && !Match(TOKEN_CLOSED_PAREN)) {
    Expression* parameter = ParseExpression(depth + 1);
    if(!parameter) {
      return NULL;
    }
    parameters->AddExpression(parameter);
    if(Match(TOKEN_COMMA)) {
      NextToken();
      if(Match(TOKEN_CLOSED_PAREN)) {
        ProcessError(L"Expected expression", parameters);
      }
    }
  }

  if(!Match(TOKEN_CLOSED_PAREN)) {
		ProcessError(TOKEN_CLOSED_PAREN);
		return NULL;
	}
  NextToken();

  return parameters;
}
