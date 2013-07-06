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
#include "scanner.h"

using namespace std;

namespace compiler {
  class TreeFactory;
  class Reference;
  class ExpressionList;

  /****************************
   * Base class for all parse nodes
   ****************************/
  class ParseNode {
   protected:
	  wstring file_name;
	  unsigned int line_num;
	
   public:
	  ParseNode(const wstring &file_name, const unsigned int line_num) {
		  this->file_name = file_name;
		  this->line_num = line_num;
	  }
  
	  virtual ~ParseNode() {
	  }
    
	  const wstring GetFileName() {
		  return file_name;
	  }

	  const int GetLineNumber() {
		  return line_num;
	  }
  };  

  /****************************
   * Expression types
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
    Expression(const wstring &file_name, const unsigned int line_num) : ParseNode(file_name, line_num) {
    }
		
    virtual ~Expression() {
    }

   public:
    virtual const ExpressionType GetExpressionType() = 0;
  };
  
  /****************************
   * CharacterString class
   ****************************/
  class CharacterString : public Expression {
    friend class TreeFactory;
    int id;
    wstring char_string;

     CharacterString(const wstring &file_name, const unsigned int line_num, const wstring &orig) 
		   : Expression(file_name, line_num) {
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

    CalculatedExpression(const wstring &file_name, const unsigned int line_num, ExpressionType t) 
		  : Expression(file_name, line_num) {
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

    BooleanLiteral(const wstring &file_name, const unsigned int line_num, bool v) : Expression(file_name, line_num) {
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

    NilLiteral(const wstring &f, const int l) : Expression(file_name, line_num) {
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
    CHAR_T value;

    CharacterLiteral(const wstring &file_name, const unsigned int line_num, CHAR_T v) 
		  : Expression(file_name, line_num) {
      value = v;
    }
	
    ~CharacterLiteral() {
    }

   public:
    CHAR_T GetValue() {
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
    INT_T value;

    IntegerLiteral(const wstring &file_name, const unsigned int line_num, INT_T v) 
		  : Expression(file_name, line_num) {
      value = v;
    }

    ~IntegerLiteral() {
    }

   public:
    INT_T GetValue() {
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
    FLOAT_T value;

    FloatLiteral(const wstring &file_name, const unsigned int line_num, FLOAT_T v) 
		  : Expression(file_name, line_num) {
      value = v;
    }

    ~FloatLiteral() {
    }

   public:
    FLOAT_T GetValue() {
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
	  int id;
    ExpressionList* indices;
    Reference* reference;
    bool is_self;
    int array_size;
    int array_dim;

	  Reference(const wstring &file_name, const unsigned int line_num, int v) 
	   : Expression(file_name, line_num) {
      name = L"@self";
		  id = v;
      is_self = true;
      reference	= NULL;
      indices = NULL;
    }
	
    Reference(const wstring &file_name, const unsigned int line_num, const wstring &n, int v) 
		  : Expression(file_name, line_num) {
      name = n;
		  id = v;
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
	
	  int GetId() {
		  return id;
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
   * StatementType enum
   ****************************/
  enum StatementType {
    ASSIGNMENT_STATEMENT = -200,
	  IF_ELSE_STATEMENT,
	  WHILE_STATEMENT,
    FOR_STATEMENT,
		DECLARATION_STATEMENT,
    DUMP_STATEMENT
  };

  /****************************
   * Statement base class
   ****************************/
  class Statement : public ParseNode {
    friend class TreeFactory;

   public:    
    Statement(const wstring &file_name, const unsigned int line_num) : ParseNode(file_name, line_num) {
    }

    ~Statement() {
    }

    virtual const StatementType GetStatementType() = 0;
  };

  /****************************
   * StatementList class
   ****************************/
  class StatementList : public ParseNode {
    friend class TreeFactory;
    vector<Statement*> statements;
  
    StatementList(const wstring &file_name, const unsigned int line_num) : ParseNode(file_name, line_num) {
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
   * Declaration statement
   ****************************/
  class Declaration : public Statement {
    friend class TreeFactory;
		wstring identifier;
		
   public:
		Declaration(const wstring &file_name, const unsigned int line_num, const wstring &identifier) 
			: Statement(file_name, line_num) {
			this->identifier = identifier;
    }
		
    const StatementType GetStatementType() {
      return DECLARATION_STATEMENT;
    }
  };
		
  /****************************
   * Dump statement
   ****************************/
  class Dump : public Statement {
    friend class TreeFactory;
    Expression* expression;
  
   public:
     Dump(const wstring &file_name, const unsigned int line_num, Expression* expression) 
		   : Statement(file_name, line_num) {
      this->expression = expression;
    }
  
    Expression* GetExpression() {
      return expression;
    }

    const StatementType GetStatementType() {
      return DUMP_STATEMENT;
    }
  };

  /****************************
   * While statement
   ****************************/
  class While : public Statement {
    friend class TreeFactory;
    Expression* logical_expression;
	  StatementList* block;
	
   public:
     While(const wstring &file_name, const unsigned int line_num, Expression* logical_expression, 
					   StatementList* block)  : Statement(file_name, line_num) {
		  this->logical_expression = logical_expression;
	    this->block = block;
	  }
	
	  Expression* GetExpression() {
		  return logical_expression;
	  }

	  StatementList* GetBlock() {
		  return block;
	  }

	  const StatementType GetStatementType() {
      return WHILE_STATEMENT;
    }
  };

  /****************************
   * If/Else statement
   ****************************/
  class IfElse : public Statement {
    friend class TreeFactory;
    Expression* logical_expression;
	  StatementList* if_block;
    vector<IfElse*> else_ifs;
    StatementList* else_block;
    
  public:
    IfElse(const wstring &file_name, const unsigned int line_num, Expression* logical_expression, 
					 StatementList* if_block)  : Statement(file_name, line_num) {
		  this->logical_expression = logical_expression;
	    this->if_block = if_block;
      else_block = NULL;
	  }

    Expression* GetExpression() {
		  return logical_expression;
	  }

	  StatementList* GetIfBlock() {
		  return if_block;
	  }

    void SetElseBlock(StatementList* else_block) {
		  this->else_block = else_block;
	  }

    StatementList* GetElseBlock() {
		  return else_block;
	  }

    void AddElseIf(IfElse* if_else) {
		  else_ifs.push_back(if_else);
	  }

    vector<IfElse*> GetElseIfs() {
		  return else_ifs;
	  }
    
	  const StatementType GetStatementType() {
      return IF_ELSE_STATEMENT;
    }
  };
  
  /****************************
   * Assignment statement
   ****************************/
  class Assignment : public Statement {
    friend class TreeFactory;
    ScannerTokenType type;
    Reference* reference;
    Expression* expression;
  
   public:
     Assignment(const wstring &file_name, const unsigned int line_num, ScannerTokenType type, Reference* reference, Expression* expression) 
		   : Statement(file_name, line_num) {
      this->type = type;
      this->reference = reference;
      this->expression = expression;
    }
  
    ~Assignment() {
    }

    ScannerTokenType GetAssignmentType() {
      return type;
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
   * ExpressionList class
   ****************************/
  class ExpressionList : public ParseNode {
    friend class TreeFactory;
    vector<Expression*> expressions;
	
    ExpressionList(const wstring &file_name, const unsigned int line_num) : ParseNode(file_name, line_num) {
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
   * Function class
   ****************************/
  class Function : public ParseNode {
    ExpressionList* parameters;
    StatementList* statements;

  public:
    Function(const wstring &file_name, const unsigned int line_num, ExpressionList* parameters, StatementList* statements) 
       : ParseNode(file_name, line_num) {
      this->parameters = parameters;
      this->statements = statements;
    }

    ~Function() {
    }


    inline ExpressionList* GetParameters() {
      return parameters;
    }

    inline StatementList* GetStatements() {
      return statements;
    }
  };

  /****************************
   * Parsed program class
   ****************************/
  class ParsedProgram {
    vector<Function*> functions;
    StatementList* statements;

  public:
    ParsedProgram() {
    }

    ~ParsedProgram() {
    }

    void AddFunction(Function* function) {
      functions.push_back(function);
    }

    void SetStatements(StatementList* statements) {
      this->statements = statements;
    }

    StatementList* GetStatements() {
      return statements;
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

    ExpressionList* MakeExpressionList(const wstring &file_name, const unsigned int line_num) {
      ExpressionList* tmp = new ExpressionList(file_name, line_num);
      expression_lists.push_back(tmp);
      return tmp;
    }

    StatementList* MakeStatementList(const wstring &file_name, const unsigned int line_num) {
      StatementList* tmp = new StatementList(file_name, line_num);
      statement_lists.push_back(tmp);
      return tmp;
    }
  
    Assignment* MakeAssignmentStatement(const wstring &file_name, const unsigned int line_num, 
																			  ScannerTokenType type, Reference* reference, Expression* expression) {
      Assignment* tmp = new Assignment(file_name, line_num, type, reference, expression);
      statements.push_back(tmp);
      return tmp;
    }

	  While* MakeWhileStatement(const wstring &file_name, const unsigned int line_num, 
																  Expression* logical_expression, StatementList* block) {
		  While* tmp = new While(file_name, line_num, logical_expression, block);
		  statements.push_back(tmp);
		  return tmp;
	  }

    IfElse* MakeIfElseStatement(const wstring &file_name, const unsigned int line_num, 
																  Expression* logical_expression, StatementList* if_block) {
		  IfElse* tmp = new IfElse(file_name, line_num, logical_expression, if_block);
		  statements.push_back(tmp);
		  return tmp;
	  }
  
    Dump* MakeDumpStatement(const wstring &file_name, const unsigned int line_num, Expression* expression) {
      Dump* tmp = new Dump(file_name, line_num, expression);
      statements.push_back(tmp);
      return tmp;
    }

    Function* MakeFunction(const wstring &file_name, const unsigned int line_num, ExpressionList* parameters, StatementList* statements) {
      Function* tmp = new Function(file_name, line_num, parameters, statements);
      nodes.push_back(tmp);
      return tmp;
    }

		Declaration* MakeDeclarationStatement(const wstring &file_name, const unsigned int line_num, 
																					const wstring &variable) {
      Declaration* tmp = new Declaration(file_name, line_num, variable);
      statements.push_back(tmp);
      return tmp;
    }
		
    CalculatedExpression* MakeCalculatedExpression(const wstring &file_name, const unsigned int line_num, 
																									 ExpressionType type) {
      CalculatedExpression* tmp = new CalculatedExpression(file_name, line_num, type);
      expressions.push_back(tmp);
      return tmp;
    }

    IntegerLiteral* MakeIntegerLiteral(const wstring &file_name, const unsigned int line_num, INT_T value) {
      IntegerLiteral* tmp = new IntegerLiteral(file_name, line_num, value);
      expressions.push_back(tmp);
      return tmp;
    }

    FloatLiteral* MakeFloatLiteral(const wstring &file_name, const unsigned int line_num, FLOAT_T value) {
      FloatLiteral* tmp = new FloatLiteral(file_name, line_num, value);
      expressions.push_back(tmp);
      return tmp;
    }

    CharacterLiteral* MakeCharacterLiteral(const wstring &file_name, const unsigned int line_num, CHAR_T value) {
      CharacterLiteral* tmp = new CharacterLiteral(file_name, line_num, value);
      expressions.push_back(tmp);
      return tmp;
    }
	
    CharacterString* MakeCharacterString(const wstring &file_name, const unsigned int line_num, 
																			   const wstring &char_string) {
		  CharacterString* tmp = new CharacterString(file_name, line_num, char_string);
		  expressions.push_back(tmp);
		  return tmp;
	  }
	
    NilLiteral* MakeNilLiteral(const wstring &file_name, const unsigned int line_num) {
      NilLiteral* tmp = new NilLiteral(file_name, line_num);
      expressions.push_back(tmp);
      return tmp;
    }

    BooleanLiteral* MakeBooleanLiteral(const wstring &file_name, const unsigned int line_num, bool boolean) {
      BooleanLiteral* tmp = new BooleanLiteral(file_name, line_num, boolean);
      expressions.push_back(tmp);
      return tmp;
    }
	
	  Reference* MakeReference(const wstring &file_name, const unsigned int line_num, int id) {
      Reference* tmp = new Reference(file_name, line_num, id);
      references.push_back(tmp);
      return tmp;
	  }
	
    Reference* MakeReference(const wstring &file_name, const unsigned int line_num, 
													   const wstring &name, int id) {
      Reference* tmp = new Reference(file_name, line_num, name, id);
      references.push_back(tmp);
      return tmp;
    }
  };
}

#endif
