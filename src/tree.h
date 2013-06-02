/***************************************************************************
 * Debugger parse tree.
 *
 * Copyright (c) 2013 Randy Hollines
 * All rights reserved.
 *
 * Redistribution and uses in source and binary forms, with or without
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

#ifndef __TREE_H__
#define __TREE_H__

#include "common.h"

using namespace std;


class TreeFactory;
class Reference;
class ExpressionList;

/****************************
 * ParseNode base class
 ****************************/
class ParseNode {
  bool is_error;

 public:
  ParseNode() {
    is_error = false;
  }

  virtual ~ParseNode() {
  }

  void SetError() {
    is_error = false;
  }

  bool IsError() {
    return is_error;
  }
};  

/****************************
 * ExpressionType enum
 ****************************/
enum ExpressionType {
  REF_EXPR = -100,
  NIL_LIT_EXPR,
  CHAR_LIT_EXPR,
  INT_LIT_EXPR,
  FLOAT_LIT_EXPR,
  BOOLEAN_LIT_EXPR,
  AND_EXPR,
  OR_EXPR,
  EQL_EXPR,
  NEQL_EXPR,
  LES_EXPR,
  GTR_EQL_EXPR,
  LES_EQL_EXPR,
  GTR_EXPR,
  ADD_EXPR,
  SUB_EXPR,
  MUL_EXPR,
  DIV_EXPR,
  MOD_EXPR,
  CHAR_STR_EXPR,
};

/****************************
 * Expression base class
 ****************************/
class Expression : public ParseNode {
  friend class TreeFactory;

 protected:    
 Expression() : ParseNode() {
  }
		
  virtual ~Expression() {
  }

 public:
  virtual const ExpressionType GetExpressionType() = 0;
};

/****************************
 * ExpressionList class
 ****************************/
class ExpressionList {
  friend class TreeFactory;
  vector<Expression*> expressions;

  ExpressionList() {
  }

  ~ExpressionList() {
  }

 public:
  vector<Expression*> GetExpressions() {
    return expressions;
  }

  void AddExpression(Expression* e) {
    expressions.push_back(e);
  }
};

/****************************
 * StatementType enum
 ****************************/
enum StatementType {
  ASSIGNMENT_STATEMENT = -200,
  DUMP_STATEMENT
};

/****************************
 * Statement base class
 ****************************/
class Statement : public ParseNode {
  friend class TreeFactory;

 public:    
  Statement() : ParseNode() {
  }

  ~Statement() {
  }

  virtual const StatementType GetStatementType() = 0;
};

/****************************
 * StatementList class
 ****************************/
class StatementList {
  friend class TreeFactory;
  vector<Statement*> statements;
  
  StatementList() {
  }
  
  ~StatementList() {
  }
  
 public:
  vector<Statement*> GetStatements() {
    return statements;
  }
  
  void AddStatement(Statement* s) {
    statements.push_back(s);
  }
};

/****************************
 * Dump statement
 ****************************/
class Dump : public Statement {
  friend class TreeFactory;
  Reference* reference;
  
 public:
  Dump(Reference* reference) : Statement() {
    this->reference = reference;
  }
  
  Reference* GetReference() {
    return reference;
  }

  const StatementType GetStatementType() {
    return DUMP_STATEMENT;
  }
};

/****************************
 * Assignment statement
 ****************************/
class Assignment : public Statement {
  friend class TreeFactory;
  Reference* reference;
  Expression* expression;
  
 public:
   Assignment(Reference* reference, Expression* expression) : Statement() {
    this->reference = reference;
    this->expression = expression;
  }
  
  ~Assignment() {
  }

  Reference* GetReference() {
    return reference;
  }
  
  Expression* GetExpression() {
    return expression;
  }
  
  const StatementType GetStatementType() {
    return ASSIGNMENT_STATEMENT;
  }
};

/****************************
 * CharacterString class
 ****************************/
class CharacterString : public Expression {
  friend class TreeFactory;
  int id;
  wstring char_string;

 CharacterString(const wstring &orig) : Expression() {
    int skip = 2;
    for(size_t i = 0; i < orig.size(); i++) {
      wchar_t c = orig[i];
      if(skip > 1 && c == L'\\' && i + 1 < orig.size()) {
        wchar_t cc = orig[i + 1];
        switch(cc) {
        case L'"':
          char_string += L'\"';
          skip = 0;
          break;

        case L'\\':
          char_string += L'\\';
          skip = 0;
          break;

        case L'n':
          char_string += L'\n';
          skip = 0;
          break;

        case L'r':
          char_string += L'\r';
          skip = 0;
          break;

        case L't':
          char_string += L'\t';
          skip = 0;
          break;

        case L'0':
          char_string += L'\0';
          skip = 0;
          break;

        default:
          if(skip > 1) {
            char_string += c;
          } else {
            skip++;
          }
          break;
        }
      }

      if(skip > 1) {
        char_string += c;
      } else {
        skip++;
      }
    }
    id = -1;
  }

  ~CharacterString() {
  }

 public:
  const ExpressionType GetExpressionType() {
    return CHAR_STR_EXPR;
  }

  void SetId(int i) {
    id = i;
  }

  int GetId() {
    return id;
  }

  const wstring& GetString() const {
    return char_string;
  }
};

/****************************
 * CalculatedExpression class
 ****************************/
class CalculatedExpression : public Expression {
  friend class TreeFactory;
  ExpressionType type;
  Expression* left;
  Expression* right;

 CalculatedExpression(ExpressionType t) :
  Expression() {
    left = right = NULL;
    type = t;
  }

  ~CalculatedExpression() {
  }

 public:
  const ExpressionType GetExpressionType() {
    return type;
  }

  void SetLeft(Expression* l) {
    left = l;
  }

  Expression* GetLeft() {
    return left;
  }

  void SetRight(Expression* r) {
    right = r;
  }

  Expression* GetRight() {
    return right;
  }
};

/****************************
 * BooleanLiteral class
 ****************************/
class BooleanLiteral : public Expression {
  friend class TreeFactory;
  bool value;

 BooleanLiteral(bool v) : Expression() {
    value = v;
  }

  ~BooleanLiteral() {
  }

 public:
  const ExpressionType GetExpressionType() {
    return BOOLEAN_LIT_EXPR;
  }

  bool GetValue() {
    return value;
  }
};

/****************************
 * NilLiteral class
 ****************************/
class NilLiteral : public Expression {
  friend class TreeFactory;

 NilLiteral(const wstring &f, const int l) : Expression() {
  }

  ~NilLiteral() {
  }

 public:
  const ExpressionType GetExpressionType() {
    return NIL_LIT_EXPR;
  }
};

/****************************
 * CharacterLiteral class
 ****************************/
class CharacterLiteral : public Expression {
  friend class TreeFactory;
  wchar_t value;

 CharacterLiteral(wchar_t v) : Expression() {
    value = v;
  }

  ~CharacterLiteral() {
  }

 public:
  wchar_t GetValue() {
    return value;
  }

  const ExpressionType GetExpressionType() {
    return CHAR_LIT_EXPR;
  }
};

/****************************
 * IntegerLiteral class
 ****************************/
class IntegerLiteral : public Expression {
  friend class TreeFactory;
  long value;

 IntegerLiteral(long v) : Expression() {
    value = v;
  }

  ~IntegerLiteral() {
  }

 public:
  long GetValue() {
    return value;
  }

  const ExpressionType GetExpressionType() {
    return INT_LIT_EXPR;
  }
};

/****************************
 * FloatLiteral class
 ****************************/
class FloatLiteral : public Expression {
  friend class TreeFactory;
  double value;

 FloatLiteral(double v) : Expression() {
    value = v;
  }

  ~FloatLiteral() {
  }

 public:
  double GetValue() {
    return value;
  }

  const ExpressionType GetExpressionType() {
    return FLOAT_LIT_EXPR;
  }
};

/****************************
 * Reference class
 ****************************/
class Reference : public Expression {
  friend class TreeFactory;
  wstring name;
  ExpressionList* indices;
  Reference* reference;
  bool is_self;
  int array_size;
  int array_dim;

 Reference() : Expression() {
    name = L"@self";
    is_self = true;
    reference	= NULL;
    array_size = 0;
    array_dim = 0;
    indices = NULL;
  }

 Reference(const wstring &v) : Expression() {
    name = v;
    is_self = false;
    reference	= NULL;
    indices = NULL;
  }

  ~Reference() {
  }

 public:
  const wstring& GetName() const {
    return name;
  }

  void SetReference(Reference* call) {
    reference = call;
  }

  Reference* GetReference() {
    return reference;
  }

  void SetIndices(ExpressionList* l) {
    indices = l;
  }
		
  ExpressionList* GetIndices() {
    return indices;
  }

  const ExpressionType GetExpressionType() {
    return REF_EXPR;
  }

  bool IsSelf() {
    return is_self;
  }

  void SetArraySize(int s) {
    array_size = s;
  }

  int GetArraySize() {
    return array_size;
  }

  void SetArrayDimension(int d) {
    array_dim = d;
  }

  int GetArrayDimension() {
    return array_dim;
  }
};

/****************************
 * TreeFactory class
 ****************************/
class TreeFactory {
  static TreeFactory* instance;
  
  vector<ParseNode*> nodes;
  vector<Expression*> expressions;
  vector<Statement*> statements;
  vector<Reference*> references;
  vector<ExpressionList*> expression_lists;
  vector<StatementList*> statement_lists;

  TreeFactory() {
  }

  ~TreeFactory() {
  }

 public:
  static TreeFactory* Instance();

  void Clear() {
    while(!nodes.empty()) {
      ParseNode* tmp = nodes.front();
      nodes.erase(nodes.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }

    while(!expressions.empty()) {
      Expression* tmp = expressions.front();
      expressions.erase(expressions.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }

    while(!statement_lists.empty()) {
      StatementList* tmp = statement_lists.front();
      statement_lists.erase(statement_lists.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }
    
    while(!statements.empty()) {
      Statement* tmp = statements.front();
      statements.erase(statements.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }

    while(!references.empty()) {
      Reference* tmp = references.front();
      references.erase(references.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }

    while(!expression_lists.empty()) {
      ExpressionList* tmp = expression_lists.front();
      expression_lists.erase(expression_lists.begin());
      // delete
      delete tmp;
      tmp = NULL;
    }

    delete instance;
    instance = NULL;
  }

  ExpressionList* MakeExpressionList() {
    ExpressionList* tmp = new ExpressionList;
    expression_lists.push_back(tmp);
    return tmp;
  }

  StatementList* MakeStatementList() {
    StatementList* tmp = new StatementList;
    statement_lists.push_back(tmp);
    return tmp;
  }
  
  Assignment* MakeAssignmentStatement(Reference* reference, Expression* expression) {
    Assignment* tmp = new Assignment(reference, expression);
    statements.push_back(tmp);
    return tmp;
  }
  
  Dump* MakeDumpStatement(Reference* reference) {
    Dump* tmp = new Dump(reference);
    statements.push_back(tmp);
    return tmp;
  }
  
  CalculatedExpression* MakeCalculatedExpression(ExpressionType type) {
    CalculatedExpression* tmp = new CalculatedExpression(type);
    expressions.push_back(tmp);
    return tmp;
  }

  IntegerLiteral* MakeIntegerLiteral(long value) {
    IntegerLiteral* tmp = new IntegerLiteral(value);
    expressions.push_back(tmp);
    return tmp;
  }

  FloatLiteral* MakeFloatLiteral(double value) {
    FloatLiteral* tmp = new FloatLiteral(value);
    expressions.push_back(tmp);
    return tmp;
  }

  CharacterLiteral* MakeCharacterLiteral(wchar_t value) {
    CharacterLiteral* tmp = new CharacterLiteral(value);
    expressions.push_back(tmp);
    return tmp;
  }

  CharacterString* MakeCharacterString(const wstring &char_string) {
    CharacterString* tmp = new CharacterString(char_string);
    expressions.push_back(tmp);
    return tmp;
  }

  NilLiteral* MakeNilLiteral(const wstring &file_name, const int line_num) {
    NilLiteral* tmp = new NilLiteral(file_name, line_num);
    expressions.push_back(tmp);
    return tmp;
  }

  BooleanLiteral* MakeBooleanLiteral(bool boolean) {
    BooleanLiteral* tmp = new BooleanLiteral(boolean);
    expressions.push_back(tmp);
    return tmp;
  }

  Reference* MakeReference() {
    Reference* tmp = new Reference();
    references.push_back(tmp);
    return tmp;
  }

  Reference* MakeReference(const wstring &v) {
    Reference* tmp = new Reference(v);
    references.push_back(tmp);
    return tmp;
  }
};

#endif
