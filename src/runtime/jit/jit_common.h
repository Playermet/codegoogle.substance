/***************************************************************************
 * Common JIT code
 *
 * Copyright (c) 2008-2013, Randy Hollines
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

#include "../../common.h"

namespace jit {
  typedef long (*jit_fun_ptr)(Value* frame, void* inst_mem, void* cls_mem);

  // vm instructions
  enum JitInstructionType {
    // loads operations
    LOAD_INT_LIT  = 0,
    LOAD_CHAR_LIT,
    LOAD_FLOAT_LIT,
    LOAD_INT_VAR,
    LOAD_LOCL_INT_VAR, // only used by the VM
    LOAD_CLS_INST_INT_VAR, // only used by the VM
    LOAD_FLOAT_VAR,
    LOAD_FUNC_VAR,
    LOAD_CLS_MEM,
    LOAD_INST_MEM,
    // stores operations
    STOR_INT_VAR,
    STOR_LOCL_INT_VAR, // only used by the VM
    STOR_CLS_INST_INT_VAR, // only used by the VM
    STOR_FLOAT_VAR,
    STOR_FUNC_VAR,
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
    MTHD_CALL,
    DYN_MTHD_CALL,
    JMP,
    LBL,
    RTRN,
    // memory operations
    NEW_BYTE_ARY,
    NEW_CHAR_ARY,
    NEW_INT_ARY,
    NEW_FLOAT_ARY,
    NEW_OBJ_INST,
    CPY_BYTE_ARY,
    CPY_CHAR_ARY,
    CPY_INT_ARY,
    CPY_FLOAT_ARY,
    // casting & type check
    OBJ_INST_CAST,
    OBJ_TYPE_OF,
    // external OS traps
    TRAP,
    TRAP_RTRN,
    // shared libraries
    DLL_LOAD,
    DLL_UNLOAD,
    DLL_FUNC_CALL,
    // stack ops
    SWAP_INT,
    POP_INT,
    POP_FLOAT,
    // thread directives
    ASYNC_MTHD_CALL,
    THREAD_JOIN,
    THREAD_SLEEP,
    THREAD_MUTEX,
    CRITICAL_START,
    CRITICAL_END,
    // library directives
    LIB_NEW_OBJ_INST,
    LIB_MTHD_CALL,
    LIB_OBJ_INST_CAST,
    LIB_FUNC_DEF,
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
}

#endif