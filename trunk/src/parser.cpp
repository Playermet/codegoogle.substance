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
 * Loads parsing error codes.
 ****************************/
void Parser::LoadErrorCodes()
{
  error_msgs[TOKEN_IDENT] = L"Expected identifier";
  error_msgs[TOKEN_OPEN_PAREN] = L"Expected '('";
  error_msgs[TOKEN_CLOSED_PAREN] = L"Expected ')'";
  error_msgs[TOKEN_OPEN_BRACKET] = L"Expected '['";
  error_msgs[TOKEN_CLOSED_BRACKET] = L"Expected ']'";
  error_msgs[TOKEN_OPEN_BRACE] = L"Expected '{'";
  error_msgs[TOKEN_CLOSED_BRACE] = L"Expected '}'";
  error_msgs[TOKEN_COLON] = L"Expected ':'";
  error_msgs[TOKEN_COMMA] = L"Expected ','";
  error_msgs[TOKEN_ASSIGN] = L"Expected '='";
  error_msgs[TOKEN_SEMI_COLON] = L"Expected ';'";
  error_msgs[TOKEN_ASSESSOR] = L"Expected '->'";
}

/****************************
 * Emits parsing error.
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
 * Emits parsing error.
 ****************************/
void Parser::ProcessError(const wstring &msg)
{
#ifdef _DEBUG
  wcout << L"\tError: " << GetFileName() << L":" << GetLineNumber() << L": "
	<< msg << endl;
#endif

  const wstring &str_line_num = ToString(GetLineNumber());
  errors.insert(pair<int, wstring>(GetLineNumber(), GetFileName() + L":" + 
				   str_line_num + L": " + msg));
}

/****************************
 * Emits parsing error.
 ****************************/
void Parser::ProcessError(const wstring &msg, ScannerTokenType sync)
{
#ifdef _DEBUG
  wcout << L"\tError: " << GetFileName() << L":" << GetLineNumber() << L": "
	<< msg << endl;
#endif

  const wstring &str_line_num = ToString(GetLineNumber());
  errors.insert(pair<int, wstring>(GetLineNumber(),
				   GetFileName() + L":" + str_line_num +
				   L": " + msg));
  ScannerTokenType token = GetToken();
  while(token != sync && token != TOKEN_END_OF_STREAM) {
    NextToken();
    token = GetToken();
  }
}

/****************************
 * Emits parsing error.
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
 * Checks for parsing errors.
 ****************************/
bool Parser::CheckErrors()
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
StatementList* Parser::Parse()
{
#ifdef _DEBUG
  wcout << L"\n========== Scanning/Parsing =========" << endl;
#endif
  NextToken();
  
  // parse input
  StatementList* block = ParseBlock(true, 0);
  if(CheckErrors()) {
    return block;
  }
  
  return NULL;
}

/****************************
 * Parses a statement block
 ****************************/
StatementList* Parser::ParseBlock(bool new_scope, int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();

	if(new_scope) {
		symbol_table.NewScope();
	}

  StatementList* block = TreeFactory::Instance()->MakeStatementList(file_name, line_num);
  
  if(!Match(TOKEN_OPEN_BRACE)) {
    ProcessError(TOKEN_OPEN_BRACE);
    return NULL;
  }
  NextToken();

  while(!Match(TOKEN_CLOSED_BRACE) && !Match(TOKEN_END_OF_STREAM)) {
		Statement* statement = ParseStatement(0);
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
		symbol_table.PreviousScope();
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
    // assignment
  case TOKEN_IDENT:
    statement = ParseAssignment(depth + 1);
    break;

		// if
  case TOKEN_IF_ID:
    statement = ParseIfWhile(true, depth + 1);
    break;
		
		// while
  case TOKEN_WHILE_ID:
    statement = ParseIfWhile(false, depth + 1);
    break;
    
    // value dump
  case TOKEN_DUMP_ID: {
#ifdef _DEBUG
    Show(L"Dump", depth);
#endif
    NextToken();
    statement  =TreeFactory::Instance()->MakeDumpStatement(file_name, line_num,  ParseExpression(depth + 1));
  }
    break;

  default:
    statement = NULL;
    ProcessError(L"Invalid statement");
    break;
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
 * Parses an 'if' and 'while' 
 * statements.
 ****************************/
Statement* Parser::ParseIfWhile(bool is_if, int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
	Show(L"If", depth);
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
		return TreeFactory::Instance()->MakeIfWhileStatement(file_name, line_num, expression, block, is_if);
	}
	
	return NULL;
}

/****************************
 * Parses an assignment statement.
 ****************************/
Statement* Parser::ParseAssignment(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Assignment", depth);
#endif
  
  if(!Match(TOKEN_IDENT)) {
    ProcessError(TOKEN_IDENT);
    return NULL;
  }
  const wstring identifier = scanner->GetToken()->GetIdentifier();
  NextToken(); 
	
	if(!symbol_table.HasEntry(identifier)) {
		symbol_table.AddEntry(identifier); 
	}
  Reference* reference = ParseReference(identifier, depth + 1);
  
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
#ifdef _DEBUG
  Show(L"Expression", depth);
#endif

  return ParseLogic(depth + 1);
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
  while((Match(TOKEN_AND) || Match(TOKEN_OR)) && !Match(TOKEN_END_OF_STREAM)) {
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
  while((Match(TOKEN_ADD) || Match(TOKEN_SUB)) && !Match(TOKEN_END_OF_STREAM)) {
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
  while((Match(TOKEN_MUL) || Match(TOKEN_DIV) || Match(TOKEN_MOD)) &&
        !Match(TOKEN_END_OF_STREAM)) {
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

    while(!Match(TOKEN_CLOSED_BRACKET) && !Match(TOKEN_END_OF_STREAM)) {
      // expression
      expressions->AddExpression(ParseExpression(depth + 1));

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
    NextToken();
    expression = ParseReference(identifier, depth + 1);
  }
  else if(Match(TOKEN_THIS_ID)) {
    NextToken();
    expression = ParseReference(depth + 1);
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
Reference* Parser::ParseReference(int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Reference", depth);
#endif

	// self reference
	const wstring identifier = L"@self";
	int entry_id = symbol_table.GetEntry(identifier);
	if(entry_id < 0) {
		ProcessError(L"Unknown refernce '" + identifier + L"'");
		return NULL;
	}
	
  Reference* inst_ref = TreeFactory::Instance()->MakeReference(file_name, line_num, entry_id);

  // subsequent instance references
  if(Match(TOKEN_ASSESSOR)) {
    ParseReference(inst_ref, depth + 1);
  }

  return inst_ref;
}

/****************************
 * Parses a instance reference.
 ****************************/
Reference* Parser::ParseReference(const wstring &identifier, int depth)
{
	const unsigned int line_num = GetLineNumber();
  const wstring &file_name = GetFileName();
	
#ifdef _DEBUG
  Show(L"Reference", depth);
#endif
	
	// self reference
	int entry_id = symbol_table.GetEntry(identifier);
	if(entry_id < 0) {
		ProcessError(L"Unknown refernce '" + identifier + L"'");
		return NULL;
	}
	
  Reference* inst_ref = TreeFactory::Instance()->MakeReference(file_name, line_num, identifier, entry_id);
  if(Match(TOKEN_OPEN_BRACKET)) {
    inst_ref->SetIndices(ParseIndices(depth + 1));
  }

  // subsequent instance references
  if(Match(TOKEN_ASSESSOR)) {
    ParseReference(inst_ref, depth + 1);
  }

  return inst_ref;
}

/****************************
 * Parses an instance reference.
 ****************************/
void Parser::ParseReference(Reference* reference, int depth)
{
#ifdef _DEBUG
  Show(L"Nested reference", depth);
#endif

  NextToken();
  if(!Match(TOKEN_IDENT)) {
    ProcessError(TOKEN_IDENT);
  }
  // identifier
  const wstring &identifier = scanner->GetToken()->GetIdentifier();
  NextToken();

  if(reference) {
    reference->SetReference(ParseReference(identifier, depth + 1));
    // subsequent instance references
    if(Match(TOKEN_ASSESSOR)) {
      ParseReference(reference->GetReference(), depth + 1);
    }
  }
}
