/***************************************************************************
 * Debugger parser.
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

#ifndef __PARSER_H__
#define __PARSER_H__

#include "scanner.h"
#include "tree.h"

#define SECOND_INDEX 1
#define THIRD_INDEX 2

namespace compiler {
  /****************************
   * Parsers source files.
   ****************************/
  class Parser {
    ParsedProgram* program;
	  wstring input;
    Scanner* scanner;
	  SymbolTable* symbol_table;
	  map<ScannerTokenType, wstring> error_msgs;
    map<size_t, wstring> errors;
	
    inline void NextToken() {
      scanner->NextToken();
    }

    inline bool Match(enum ScannerTokenType type, int index = 0) {
      return scanner->GetToken(index)->GetType() == type;
    }

	  inline int GetLineNumber() {
      return scanner->GetToken()->GetLineNumber();
    }

    inline const wstring GetFileName() {
      return scanner->GetToken()->GetFileName();
    }
	
    inline enum ScannerTokenType GetToken(int index = 0) {
      return scanner->GetToken(index)->GetType();
    }

    void Show(const wstring &msg, int depth) {
      for(int i = 0; i < depth; i++) {
        wcout << L"  ";
      }
      wcout << msg << endl;
    }
  
    inline wstring ToString(int v) {
      wostringstream str;
      str << v;
      return str.str();
    }
	
    // error processing
    void LoadErrorCodes();
    void ProcessError(const ScannerTokenType type);
    void ProcessError(const wstring &msg);
    void ProcessError(const wstring &msg, ParseNode* node);
    void ProcessError(const wstring &msg, const ScannerTokenType sync);
    bool NoErrors();
	
    // parsing operations
    ParsedClass* ParseClass(int depth);
    ParsedFunction* ParseFunction(int depth);    
    ExpressionList* ParseDeclarationParameters(int depth);
    StatementList* ParseBlock(bool new_scope, int depth);
		Statement* ParseDeclaration(int depth);
	  Statement* ParseStatement(int depth);
    Statement* ParseIfElse(int depth);
	  Statement* ParseWhile(int depth);
    Statement* ParseAssignment(Reference* reference, ScannerTokenType type, int depth);
	  ExpressionList* ParseIndices(int depth);
    Expression* ParseExpression(int depth);
    Expression* ParseLogic(int depth);
    Expression* ParseMathLogic(int depth);
    Expression* ParseTerm(int depth);
    Expression* ParseFactor(int depth);
    Expression* ParseSimpleExpression(int depth);
    Reference* ParseSelf(int depth);
    Reference* ParseReference(const wstring &ident, int depth);
    void ParseReference(Reference* reference, int depth);
		ExpressionList* ParseCallingParameters(int depth);
  
   public:
    Parser(const wstring &input) {
		  this->input = input;
      LoadErrorCodes();
		  scanner = new Scanner(input);
    }
  
    ~Parser() {
		  if(scanner) {
			  delete scanner;
			  scanner = NULL;
		  }
    }

    void DeleteProgram() {
      if(program) {
        delete program;
        program = NULL;
      }
    }
  
    ParsedProgram* Parse();
  };
}

#endif
