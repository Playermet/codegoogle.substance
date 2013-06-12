/***************************************************************************
 * Common JIT compiler framework
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
 * - Neither the name of the Objeck Team nor the names of its 
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
#ifndef __JIT_COMMON_H__
#define __JIT_COMMON_H__

#include <sys/mman.h>
#include <errno.h>
#include "../os/posix/memory.h"
#include "../os/posix/posix.h"
#include "../common.h"
#include "../interpreter.h"

using namespace std;

namespace jit {
  class JitIR;

  typedef long (*jit_fun_ptr)(Value* frame, void* inst_mem, void* cls_mem);

  // vm instructions
  enum JitInstructionType {
    // loads operations
    LOAD_INT_LIT  = 0,
    LOAD_CHAR_LIT,
    LOAD_FLOAT_LIT,
    LOAD_INT_VAR,
    LOAD_FLOAT_VAR,
    LOAD_CLS_MEM,
    LOAD_INST_MEM,
    // stores operations
    STOR_INT_VAR,
    STOR_FLOAT_VAR,
    // array operations
    LOAD_BYTE_ARY_ELM,
    LOAD_CHAR_ARY_ELM,
    LOAD_INT_ARY_ELM,
    LOAD_FLOAT_ARY_ELM,
    STOR_BYTE_ARY_ELM,
    STOR_CHAR_ARY_ELM,
    STOR_INT_ARY_ELM,
    STOR_FLOAT_ARY_ELM,
	  LOAD_ARY_SIZE,
    // logical operations
    EQL_INT,
    NEQL_INT,
    LES_INT,
    GTR_INT,
    LES_EQL_INT,
    GTR_EQL_INT,
    EQL_FLOAT,
    NEQL_FLOAT,
    LES_FLOAT,
    GTR_FLOAT,
    LES_EQL_FLOAT,
    GTR_EQL_FLOAT,
    // mathematical operations
    AND_INT,
    OR_INT,
    ADD_INT,
    SUB_INT,
    MUL_INT,
    DIV_INT,
    MOD_INT,
    BIT_AND_INT,
    BIT_OR_INT,
    BIT_XOR_INT,
    SHL_INT,
    SHR_INT,
    ADD_FLOAT,
    SUB_FLOAT,
    MUL_FLOAT,
    DIV_FLOAT,
    FLOR_FLOAT,
    CEIL_FLOAT,
    SIN_FLOAT,
    COS_FLOAT,
    TAN_FLOAT,
    ASIN_FLOAT,
    ACOS_FLOAT,
    ATAN_FLOAT,
    LOG_FLOAT,
    POW_FLOAT,
    SQRT_FLOAT,
    RAND_FLOAT,
    // conversions
    I2F,
    F2I,
    // control
    JMP,
    LBL,
    RTRN,
    // memory operations
    NEW_BYTE_ARY,
    NEW_CHAR_ARY,
    NEW_INT_ARY,
    NEW_FLOAT_ARY,
    NEW_OBJ_INST,
    // external OS traps
    TRAP,
    // stack ops
    SWAP_INT,
    POP_INT,
    POP_FLOAT,
    // system directives
    GUARD,
    END_STMTS
  };

  // memory reference context, used for
  // loading and storing variables
  enum JitMemoryContext {
    CLS = -3500,
    INST,
    LOCL
  };

  /********************************
   * JIT instruction class
   ********************************/
  class JitInstruction {
    JitInstructionType type;
    INT_T operand;
    INT_T operand2;
    INT_T operand3;
    FLOAT_T float_operand;
    long native_offset;
  
   public:
    JitInstruction(JitInstructionType t) {
      type = t;
      operand = operand3 = native_offset = 0;
    }

    JitInstruction(JitInstructionType t, long o) {
      type = t;
      operand = o;
      operand3 = native_offset = 0;
    }

    JitInstruction(JitInstructionType t, FLOAT_T fo) {
      type = t;
      float_operand = fo;
      operand = operand3 = native_offset = 0;
    }

    JitInstruction(JitInstructionType t, long o, long o2) {
      type = t;
      operand = o;
      operand2 = o2;
      operand3 = native_offset = 0;
    }

    JitInstruction(JitInstructionType t, long o, long o2, long o3) {
      type = t;
      operand = o;
      operand2 = o2;
      operand3 = o3;
      native_offset = 0;
    }

    ~JitInstruction() {
    }  

    inline JitInstructionType GetType() const {
      return type;
    }

    inline void SetType(JitInstructionType t) {
      type = t;
    }

    inline long GetOperand() const {
      return operand;
    }

    inline long GetOperand2() const {
      return operand2;
    }

    inline long GetOperand3() const {
      return operand3;
    }

    inline void SetOperand(long o) {
      operand = o;
    }

    inline void SetOperand2(long o2) {
      operand2 = o2;
    }

    inline void SetOperand3(long o3) {
      operand3 = o3;
    }

    inline FLOAT_T GetFloatOperand() const {
      return float_operand;
    }

    inline long GetOffset() const {
      return native_offset;
    }

    inline void SetOffset(long o) {
      native_offset = o;
    }
  };

  /********************************
   * JIT compile code
   ********************************/
  class NativeCode 
  {
    unsigned char* code;
    long size;
    FLOAT_T* floats;

   public:
    NativeCode(unsigned char* c, long s, FLOAT_T* f) {
      code = c;
      size = s;
      floats = f;
    }

    ~NativeCode() {
  #ifdef _WIN32
      free(code);
      code = NULL;
  #endif

  #ifdef _X64
      free(code);
      code = NULL;
  #endif

  #ifndef _WIN32
      free(floats);
  #else
      delete[] floats;
  #endif
      floats = NULL;
    }

    unsigned char* GetCode() const {
      return code;
    }

    long GetSize() {
      return size;
    }

    FLOAT_T* GetFloats() const {
      return floats;
    }
  };
  
  /********************************
   * 3-address subexpression class
   ********************************/
  class SubExpression {
    string key;
    StackInstr* instr;
    SubExpression* left;
    SubExpression* operation;
    SubExpression* right;
    vector<SubExpression*> sub_expressions;
    long float_index;
    bool is_temp;
    // TODO: not thread safe!!
    static long temp_id;
    
    const string LongToString(long v) {
      ostringstream str;
      str << v;
      return str.str();
    }
    
  public:
    SubExpression(StackInstr* i, bool t) {
      instr = i;
      left = this;
      operation = NULL;
      right = NULL;
      is_temp = t;
      float_index = -1;
    }

    SubExpression(StackInstr* i, long f) {
      instr = i;
      left = this;
      operation = NULL;
      right = NULL;
      is_temp = false;
      float_index = f;
    }

    SubExpression(SubExpression* l, SubExpression* o, SubExpression* r) {
      instr = NULL;
      left = l;
      operation = o;
      right = r;
      is_temp = false;
      float_index = -1;
    }

    ~SubExpression() {
    }

    inline StackInstr* GetInstruction() {
      return instr;
    }

    inline void SetInstruction(StackInstr* i) {
      instr = i;
    }
    
    inline SubExpression* GetLeft() {
      return left;
    }

    inline void SetLeft(SubExpression* l) {
      left = l;
    }
    
    inline SubExpression* GetRight() {
      return right;
    }
    
    inline void SetRight(SubExpression* r) {
      right = r;
    }
    
    inline SubExpression* GetOperation() {
      return operation;
    }

    inline long GetFloatIndex() {
      return float_index;
    }
    
    void AddSubExpression(SubExpression* s) {
      sub_expressions.push_back(s);
    }
    
    vector<SubExpression*>& GetSubExpressions() {
      return sub_expressions;
    }
    
    const string GetKey() {
      if(key.size() > 0) {
	return key;
      }
      
      // single-term expression
      if(instr) {
	switch(instr->GetType()) {
	case LOAD_INT_ARY_ELM:
	case STOR_INT_ARY_ELM: {
	  key += sub_expressions[0]->GetKey();
	  key += '@';
	  for(unsigned long i = 1; i < sub_expressions.size(); i++) {
	    key += sub_expressions[i]->GetKey();
	    if(i < sub_expressions.size() - 1) {
	      key += ',';
	    }
	  }
	}
	  break;

	case LOAD_INT_LIT:
	  key += LongToString(instr->GetOperand());
	  break;
	
	case LOAD_INT_VAR:
	case STOR_INT_VAR:
	  if(is_temp) {
	    key += 't';
	  }
	  else {
	    key += 'v';
	  }
	  
	  if(instr->GetOperand2() == LOCL) {
	    key += 'l';
	  }
	  else if(instr->GetOperand2() == INST) {
	    key += 'i';
	  }
	  else {
	    key += 'c';
	  }
	  key += LongToString(instr->GetOperand());
	  break;
	
	case JMP:
	  key += 'j';
	  key += LongToString(temp_id++);
	  break;
	   
	case RTRN:
	  key += 'r';
	  key += LongToString(temp_id++);
	  break;
	  
	case MTHD_CALL:
	  key += 'x';
	  key += LongToString(temp_id++);
	  break;
	  
	case TRAP:
	  key += 'p';
	  key += LongToString(temp_id++);
	  break;

	case LOAD_INST_MEM:
	  key += 'i';
	  break;

	default:
	  break;
	}
      }
      // two-term expression
      else {
	// left
	switch(left->GetInstruction()->GetType()) {
	case LOAD_INT_ARY_ELM:
	case STOR_INT_ARY_ELM: {
	  key += left->GetSubExpressions()[0]->GetKey();
	  key += '@';
	  for(unsigned long i = 1; i < left->GetSubExpressions().size(); i++) {
	    key += left->GetSubExpressions()[i]->GetKey();
	    if(i < left->GetSubExpressions().size() - 1) {
	      key += ',';
	    }
	  }
	}
	  break;
	  
	case LOAD_INT_LIT:
	  key += LongToString(left->GetInstruction()->GetOperand());
	  break;
	  
	case LOAD_INT_VAR:
	  if(is_temp) {
	    key += 't';
	  }
	  else {
	    key += 'v';
	  }

	  if(left->GetInstruction()->GetOperand2() == LOCL) {
	    key += 'l';
	  }
	  else if(left->GetInstruction()->GetOperand2() == INST) {
	    key += 'i';
	  }
	  else {
	    key += 'c';
	 } 
	  key += LongToString(left->GetInstruction()->GetOperand());
	  break;
	  
	case STOR_INT_VAR:
	  key += left->GetKey();
	  break;

	default:
	  break;
	}
	
	// operation
	switch(operation->GetInstruction()->GetType()) {
	case ADD_INT:
	  key += '+';
	  break;
	  
	case SUB_INT:
	  key += '-';
	  break;
	  
	case MUL_INT:
	  key += '*';
	  break;

	case DIV_INT:
	  key += '/';
	  break;

	case MOD_INT:
	  key += '%';
	  break;
	  
	case LES_INT:
	  key += '<';
	  break;
	  
	case LES_EQL_INT:
	  key += "<=";
	  break;
	  
	case GTR_INT:
	  key += '>';
	  break;

	case GTR_EQL_INT:
	  key += ">=";
	  break;
	  
	case EQL_INT:
	  key += '=';
	  break;

	case NEQL_INT:
	  key += "<>";
	  break;

	case AND_INT:
	  key += '&';
	  break;

	case OR_INT:
	  key += '|';
	  break;
	  
#ifdef _DEBUG
	default:
	  cerr << "unknown IR operation!" << endl;
	  exit(1);
#endif
	}

	// right
	switch(right->GetInstruction()->GetType()) {
	case LOAD_INT_ARY_ELM:
	case STOR_INT_ARY_ELM: {
	  key += right->GetSubExpressions()[0]->GetKey();
	  key += '@';
	  for(unsigned long i = 1; i < right->GetSubExpressions().size(); i++) {
	    key += right->GetSubExpressions()[i]->GetKey();
	    if(i < right->GetSubExpressions().size() - 1) {
	      key += ',';
	    }
	  }
	}
	  break;
	  
	case LOAD_INT_LIT:
	  key += LongToString(right->GetInstruction()->GetOperand());
	  break;
	  
	case LOAD_INT_VAR:
	  if(is_temp) {
	    key += 't';
	  }
	  else {
	    key += 'v';
	  }

	  if(right->GetInstruction()->GetOperand2() == LOCL) {
	    key += 'l';
	  }
	  else if(right->GetInstruction()->GetOperand2() == LOCL) {
	    key += 'l';
	  }
	  else {
	    key += 'c';
	  }
	  key += LongToString(right->GetInstruction()->GetOperand());
	  break;
	  
	case STOR_INT_VAR:
	  key += right->GetKey();
	  break;

	default:
	  break;
	}
      }
      
      return key;
    }
    
#ifdef _DEBUG
    string Debug() {
      string debug;
      if(instr && instr->GetType() == LBL) {
	debug += "lbl id=";
	debug += LongToString(instr->GetOperand());
      }
      else {
	debug += GetKey();
      }

      return debug;
    }
#endif
  };
  
  /********************************
   * 3-address expression class
   ********************************/
  class Expression {
    SubExpression* left;
    SubExpression* right;
    vector<Expression*> parameters;
    long instr_index;
    
    const string LongToString(long v) {
      ostringstream str;
      str << v;
      return str.str();
    }
    
  public:
    Expression(SubExpression* l) {
      left = l;
      right = NULL;
      instr_index = -1;
    }

    Expression(SubExpression* l, SubExpression* r) {
      left = l;
      right = r;
      instr_index = -1;
    }

    ~Expression() {
      while(!parameters.empty()) {
	Expression* tmp = parameters.front();
	parameters.erase(parameters.begin());
	// delete
	delete tmp;
	tmp = NULL;
      }
    }

    void SetIndex(long i) {
      instr_index = i;
    }

    long GetIndex() {
      return instr_index;
    }
    
    inline SubExpression* GetLeft() {
      return left;
    }
    
    inline SubExpression* GetRight() {
      return right;
    }

    inline void SetRight(SubExpression* r) {
      right = r;
    }

    void AddParameter(Expression* p) {
      parameters.push_back(p);
    }
    
    vector<Expression*>& GetParameters() {
      return parameters;
    }
    
    bool IsUsed(const string &key) {
      if(right) {
	if(right->GetLeft()) {
	  if(right->GetLeft()->GetKey() == key) {
	    return true;
	  }

	  vector<SubExpression*> array_exprs = right->GetLeft()->GetSubExpressions();
	  for(unsigned long i = 0; i < array_exprs.size(); i++) {
	    if(array_exprs[i]->GetKey() == key) {
	      return true;
	    }
	  }
	}
	
	if(right->GetRight()) {
	  if(right->GetRight()->GetKey() == key) {
	    return true;
	  }
	  
	  vector<SubExpression*> array_exprs = right->GetRight()->GetSubExpressions();
	  for(unsigned long i = 0; i < array_exprs.size(); i++) {
	    if(array_exprs[i]->GetKey() == key) {
	      return true;
	    }
	  }
	}
      }

      if(left) {
	if(left->GetKey() == key) {	   
	  return true;
	}

	vector<SubExpression*> array_exprs = left->GetSubExpressions();
	for(unsigned long i = 0; i < array_exprs.size(); i++) {
	  if(array_exprs[i]->GetKey() == key) {
	    return true;
	  }
	}
      }   
      
      return false;
    }
    
#ifdef _DEBUG
    string Debug() {
      string debug;
      
      debug += left->Debug();
      if(right) {
	debug += '=';
	debug += right->Debug();
      }
      
      if(left->GetInstruction()) {
	if(left->GetInstruction()->GetType() == JMP) {
	  debug += "\njmp id=";
	  debug += LongToString(left->GetInstruction()->GetOperand());
	}
	else if(left->GetInstruction()->GetType() == MTHD_CALL) {
	  debug += "={";
	  for(vector<Expression*>::reverse_iterator iter = parameters.rbegin(); 
	      iter < parameters.rend(); iter++) {
	    Expression* parameter = (*iter);
	    debug += "\n ";
	    debug += parameter->Debug();
	  }
	  debug += "\n}";
	}
      }

      return debug;
    }
#endif
  };
  
  /********************************
   * BasicBlock class
   ********************************/
  class BasicBlock {
    long id;
    JitIR* jit_ir;
    vector<Expression*> expressions;
    multimap<const string, Expression*> value_numbers;
    vector<StackInstr*> new_instrs;
    vector<BasicBlock*> successors;
    vector<BasicBlock*> predecessors;
    
  public:
    BasicBlock(long i, JitIR* j) {
      id = i;
      jit_ir = j;
    }
    
    ~BasicBlock() {
      while(!expressions.empty()) {
	Expression* tmp = expressions.front();
	expressions.erase(expressions.begin());
	// delete
	delete tmp;
	tmp = NULL;
      }
      
      while(!new_instrs.empty()) {
	StackInstr* tmp = new_instrs.front();
	new_instrs.erase(new_instrs.begin());
	// delete
	delete tmp;
	tmp = NULL;
      }
    }

    inline long GetId() {
      return id;
    }

    inline void AddSuccessor(BasicBlock* b) {
      successors.push_back(b);
    }
    
    inline void AddPredecessor(BasicBlock* b) {
      predecessors.push_back(b);
    }

    inline bool CanExtend() {
      return predecessors.size() < 2;
    }

    BasicBlock* GetDirectPredecessor() {
#ifdef _DEBUG
      assert(predecessors.size() < 2);
#endif
      return predecessors[0];
    }
    
    inline bool IsEmpty() {
      return expressions.empty();
    }

    inline bool MustWriteBack() {
      if(successors.size() == 0) {
	return true;
      }

      for(unsigned long i = 0; i < successors.size(); i++) {
	if(!successors[i]->CanExtend()) {
	  return true;
	}
      }
      
      return false;
    }
    
    inline vector<Expression*>& GetExpressions() {
      return expressions;
    }
    
    void AddExpression(Expression* e, bool can_add = true);
    
#ifdef _DEBUG
    void Debug() {
      cout << "===== (id=" << id << ") =====" << endl;
      for(unsigned long i = 0; i < expressions.size(); i++) {
	cout << expressions[i]->Debug() << endl;
      }
    }
#endif
  };
  
  /********************************
   * JitIR class
   ********************************/
  class JitIR {
    StackProgram* program;
    StackMethod* method;
    vector<BasicBlock*> blocks;    
    stack<SubExpression*> working_stack;
    BasicBlock* cur_block;
    vector<SubExpression*> expressions;
    vector<StackInstr*> new_instrs;
    map<long, BasicBlock*> labels;
    long block_id;
    long temp_id;
    vector<double> floats;
    
    void CreateBasicBlocks();    
    SubExpression* ProcessSubExpressionCalculation(SubExpression* s);
    
    inline void NewBlock() {
      if(!cur_block->IsEmpty()) {
#ifdef _DEBUG
	cur_block->Debug();
#endif
	blocks.push_back(cur_block);
	cur_block = new BasicBlock(block_id++, this);
      }
    }
    
  public: 
    JitIR(StackProgram* p) {
      program = p;
      temp_id = block_id = 0;
    }
    
    ~JitIR() {
      while(!blocks.empty()) {
	BasicBlock* tmp = blocks.front();
	blocks.erase(blocks.begin());
	// delete
	delete tmp;
	tmp = NULL;
      }

      while(!expressions.empty()) {
	SubExpression* tmp = expressions.front();
	expressions.erase(expressions.begin());
	// delete
	delete tmp;
	tmp = NULL;
      }
      
      while(!new_instrs.empty()) {
	StackInstr* tmp = new_instrs.front();
	new_instrs.erase(new_instrs.begin());
	// delete
	delete tmp;
	tmp = NULL;
      }
    }

    SubExpression* MakeSubExpression(StackInstr* i, long f) {
      SubExpression* expression = new SubExpression(i, f);
      expressions.push_back(expression);
      return expression;
    }
    
    SubExpression* MakeSubExpression(StackInstr* i, bool is_temp = false) {
      SubExpression* expression = new SubExpression(i, is_temp);
      expressions.push_back(expression);
      return expression;
    }

    SubExpression* MakeSubExpression(SubExpression* l, SubExpression* o, SubExpression* r) {
      SubExpression* expression = new SubExpression(l, o, r);
      expressions.push_back(expression);
      return expression;
    }

    inline vector<double>& GetFloats() {
      return floats;
    }
    
    vector<BasicBlock*>& CreateJitIR(StackMethod* m) {
      method = m;
      long cls_id = method->GetClass()->GetId();
      long mthd_id = method->GetId();
	
#ifdef _DEBUG
      cout << "======== JIT IR Productions: method_id=" << cls_id << "," 
	   << mthd_id << "; mthd_name='" << method->GetName() << "'; params=" 
	   << method->GetParamCount() << " ========" << endl;
#endif	
      // tranlsate program
      cur_block = new BasicBlock(block_id++, this);
      CreateBasicBlocks();
	
      // release our lock, native code has been compiled and set
      // pthread_mutex_unlock(&cm->jit_mutex);
      
      if(!cur_block->IsEmpty()) {
#ifdef _DEBUG
	cur_block->Debug();
#endif
	blocks.push_back(cur_block);
      }
      else {
	delete cur_block;
	cur_block = NULL;
      }
      
#ifdef _DEBUG
      assert(working_stack.empty());
      cout << "\nControl Flow Graph:" << endl;
#endif
      // link CFG
      for(unsigned int i = 0; i < blocks.size(); i++) {
	BasicBlock* indexed_block = blocks[i];
	vector<Expression*> expressions = indexed_block->GetExpressions();
	for(unsigned int j = 0; j < expressions.size(); j++) {
	  SubExpression* sub_expression = expressions[j]->GetLeft();
	  // map jumps
	  if(sub_expression->GetInstruction() && 
	     sub_expression->GetInstruction()->GetType() == JMP) {
	    map<long, BasicBlock*>::iterator result = labels.find(sub_expression->GetInstruction()->GetOperand());

	    if(result != labels.end()) {

#ifdef _DEBUG
	      cout << "\tjump linking: blocks " << indexed_block->GetId() << "->" 
		   << result->second->GetId() << endl;
#endif
	      indexed_block->AddSuccessor(result->second);
	      result->second->AddPredecessor(indexed_block);
	    }
	  }
	  // map last instruction 
	  if(j == expressions.size() - 1 && i + 1 < blocks.size()) {
	    if(sub_expression->GetInstruction() && 
	       sub_expression->GetInstruction()->GetType() == JMP) {
	      // conditonal jump
	      if(sub_expression->GetInstruction()->GetOperand2() > -1) {
#ifdef _DEBUG
		cout << "\tlinking: blocks " << indexed_block->GetId() << "->" 
		     << blocks[i + 1]->GetId() << endl;
#endif
		indexed_block->AddSuccessor(blocks[i + 1]);
		blocks[i + 1]->AddPredecessor(indexed_block);
	      }
	    }
	    // other
	    else {
#ifdef _DEBUG
	      cout << "\tlinking: blocks " << indexed_block->GetId() << "->" 
		   << blocks[i + 1]->GetId() << endl;
#endif
	      indexed_block->AddSuccessor(blocks[i + 1]);
	      blocks[i + 1]->AddPredecessor(indexed_block);
	    }
	  }
	}
      }
#ifdef _DEBUG
      cout << endl;
#endif

      return blocks;
    }
  };    
}

#endif  
