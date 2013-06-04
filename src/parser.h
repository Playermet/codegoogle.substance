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

/****************************
 * Local symbol table
 ****************************/
class InnerTable {
	unordered_map<wstring, Value*> table;
	
 public:
	InnerTable() {
	}
	
	~InnerTable() {
		unordered_map<wstring, Value*>::iterator iter;
		for(iter = table.begin(); iter != table.end(); ++iter) {
			delete[] iter->second;
			iter->second = NULL;			
		}
	}
	
	void AddEntry(const wstring &name) {
		table.insert(pair<wstring, Value*>(name, new Value));
	}

	Value* GetEntry(const wstring &name) {
		unordered_map<wstring, Value*>::iterator result = table.find(name);
		if(result != table.end()) {
			return result->second;
		}
		
		return NULL;
	}
	
	bool HasValue(const wstring &name) {
		return GetEntry(name) != NULL;
	}
};

/****************************
 * Hierarchical symbol table
 ****************************/
class SymbolTable {
	deque<InnerTable*> table_hierarchy;
	vector<InnerTable*> all_tables;
	
 public:
	SymbolTable() {
	}
	
	~SymbolTable() {
		while(!all_tables.empty()) {
      InnerTable* tmp = all_tables.front();
      all_tables.erase(all_tables.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }
	}
	
	void NewScope() {
		InnerTable* table = new InnerTable;
		table_hierarchy.push_front(table);
		all_tables.push_back(table);
	}
	
	bool PreviousScope() {
		if(!table_hierarchy.empty()) {
			table_hierarchy.pop_front();
			return true;
		}

		return false;
	}
	
	bool AddEntry(const wstring &name) {
		if(!table_hierarchy.empty()) {
			table_hierarchy.front()->AddEntry(name);
			return true;
		}

		return false;
	}
	
	Value* GetEntry(const wstring &name) {
		for(size_t i = 0; i < table_hierarchy.size(); ++i) {
			Value* value = table_hierarchy[i]->GetEntry(name);
			if(value) {
				return value;
			}
		}
		
		return NULL;
	}
	
	bool HasValue(wstring &name) {
		return GetEntry(name) != NULL;
	}
};	

/****************************
 * Parsers source files.
 ****************************/
class Parser {
	wstring input;
  Scanner* scanner;
	SymbolTable symbol_table;
	map<TokenType, wstring> error_msgs;
  map<int, wstring> errors;
	
  inline void NextToken() {
    scanner->NextToken();
  }

  inline bool Match(enum TokenType type, int index = 0) {
    return scanner->GetToken(index)->GetType() == type;
  }

	inline int GetLineNumber() {
    return scanner->GetToken()->GetLineNumber();
  }

  inline const wstring GetFileName() {
    return scanner->GetToken()->GetFileName();
  }
	
  inline enum TokenType GetToken(int index = 0) {
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
  void ProcessError(const TokenType type);
  void ProcessError(const wstring &msg);
  void ProcessError(const wstring &msg, ParseNode* node);
  void ProcessError(const wstring &msg, const TokenType sync);
  bool CheckErrors();
	
  // parsing operations
  StatementList* ParseBlock(bool new_scope, int depth);
	Statement* ParseStatement(int depth);
	Statement* ParseIfWhile(bool is_if, int depth);
  Statement* ParseAssignment(int depth);
	ExpressionList* ParseIndices(int depth);
  Expression* ParseExpression(int depth);
  Expression* ParseLogic(int depth);
  Expression* ParseMathLogic(int depth);
  Expression* ParseTerm(int depth);
  Expression* ParseFactor(int depth);
  Expression* ParseSimpleExpression(int depth);
  Reference* ParseReference(int depth);
  Reference* ParseReference(const wstring &ident, int depth);
  void ParseReference(Reference* reference, int depth);
  
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
  
  StatementList* Parse();
};

#endif
