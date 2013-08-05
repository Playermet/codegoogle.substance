/***************************************************************************
 * JIT compiler for the AMD64 architecture.
 *
 * Copyright (c) 2008-2013 Randy Hollines
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
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ***************************************************************************/

#ifndef __JIT_AMD64_H__
#define __JIT_AMD64_H__

#include "../../../common.h"
#include "../../jit_common.h"
#include <sys/mman.h>
#include <errno.h>

using namespace std;

// offsets for Intel (AMD-64) addresses
#define FRAME -8
#define INSTANCE_MEM -16
#define CLASS_MEM -24

// integer temps
#define TMP_REG_0 -32
#define TMP_REG_1 -40
#define TMP_REG_2 -48

#define RED_ZONE -128  
#define MAX_DBLS 64
#define PAGE_SIZE 4096
#define VALUE_OFFSET sizeof(long)

// register type
namespace jit {
  typedef enum _RegType { 
    IMM_INT = -4000,
    REG_INT,
    MEM_INT,
    IMM_FLOAT,
    REG_FLOAT,
    MEM_FLOAT,
  } RegType;
  
  // general and SSE registers
  typedef enum _Register { 
    RAX = -5000, 
    RBX, 
    RCX, 
    RDX, 
    RDI,
    RSI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    XMM0, 
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
  } Register;

  /********************************
   * RegisterHolder class
   ********************************/
  class RegisterHolder {
    Register reg;
  
  public:
    RegisterHolder(Register r) {
      reg = r;
    }

    ~RegisterHolder() {
    }

    Register GetRegister() {
      return reg;
    }
  };

  /********************************
   * RegInstr class
   ********************************/
  class RegInstr {
    RegType type;
    long operand;
    RegisterHolder* holder;
    JitInstruction* instr;
    
  public:    
    RegInstr(RegisterHolder* h) {
      if(h->GetRegister() < XMM0) {
				type = REG_INT;
      }
      else {
				type = REG_FLOAT;
      }
      holder = h;
      instr = NULL;
    }
  
    RegInstr(JitInstruction* si, double* da) {
      type = IMM_FLOAT;
      operand = (long)da;
      holder = NULL;
      instr = NULL;
    }
    
    RegInstr(RegType t, long o) {
      type = t;
      operand = o;
    }
    
    RegInstr(JitInstruction* si) {
      switch(si->GetType()) {
      case LOAD_CHAR_LIT:
      case LOAD_INT_LIT:
				type = IMM_INT;
				operand = si->GetOperand();
				break;

      case LOAD_CLS_MEM:
				type = MEM_INT;
				operand = CLASS_MEM;
				break;
	
      case LOAD_INST_MEM:
				type = MEM_INT;
				operand = INSTANCE_MEM;
				break;

      case LOAD_INT_VAR:
      case STOR_INT_VAR:
				type = MEM_INT;
				operand = si->GetOperand3();
				break;

      case LOAD_FLOAT_VAR:
      case STOR_FLOAT_VAR:
				type = MEM_FLOAT;
				operand = si->GetOperand3();
				break;

      default:
#ifdef _DEBUG
				assert(false);
#endif
				break;
      }
      instr = si;
      holder = NULL;
    }
  
    ~RegInstr() {
    }
  
    JitInstruction* GetInstruction() {
      return instr;
    }

    RegisterHolder* GetRegister() {
      return holder;
    }

    void SetType(RegType t) {
      type = t;
    }
  
    RegType GetType() {
      return type;
    }

    void SetOperand(long o) {
      operand = o;
    }

    long GetOperand() {
      return operand;
    }
  };
  
  /********************************
   * JitCompiler class
   ********************************/
  class JitCompiler {
    vector<JitInstruction*> block_instrs;
    unordered_map<INT_T, size_t>* jump_table;
    deque<RegInstr*> working_stack;
    vector<RegisterHolder*> aval_regs;
    list<RegisterHolder*> used_regs;
    stack<RegisterHolder*> aux_regs;
    vector<RegisterHolder*> aval_xregs;
    list<RegisterHolder*> used_xregs;
    unordered_map<long, JitInstruction*> jump_labels;
    unordered_map<long, JitInstruction*> native_jump_table;
    long local_space;
    long instr_count;
    unsigned char* code;
    long code_index;   
    double* floats;     
    long floats_index;
    size_t instr_index;
    long code_buf_max;
    bool compile_success;
    bool skip_jump;
    INT_T end_label;

    /********************************
     * Add byte code to buffer
     ********************************/
    void AddMachineCode(unsigned char b) {
      if(code_index == code_buf_max) {
				unsigned char* tmp;	
				if(posix_memalign((void**)&tmp, PAGE_SIZE, code_buf_max * 2)) {
					wcerr << L"Unable to reallocate JIT memory!" << endl;
					exit(1);
				}
				memcpy(tmp, code, code_index);
				free(code);
				code = tmp;
				code_buf_max *= 2;
				if(mprotect(code, code_buf_max, PROT_READ | PROT_WRITE | PROT_EXEC) < 0) {
					wcerr << L"Unable to mprotect" << endl;
					exit(1);
				}
      }
      code[code_index++] = b;
    }
    
    /********************************
     * Encodes and writes out 32-bit
     * integer values; note sizeof(int)
     ********************************/
    inline void AddImm(int imm) {
      unsigned char buffer[sizeof(int)];
      ByteEncode32(buffer, imm);
      for(size_t i = 0; i < sizeof(int); i++) {
				AddMachineCode(buffer[i]);
      }
    }
    
    /********************************
     * Encodes and writes out 64-bit
     * integer values
     ********************************/
    inline void AddImm64(long imm) {
      unsigned char buffer[sizeof(long)];
      ByteEncode64(buffer, imm);
      for(size_t i = 0; i < sizeof(long); i++) {
				AddMachineCode(buffer[i]);
      }
    }
  
    
    /********************************
     * Encoding for AMD64 "B" bits
     ********************************/
    inline unsigned char B(Register b) {
      if((b > RSP && b < XMM0) || b > XMM7) {
				return 0x49;
      }
      
      return 0x48;
    }

    /********************************
     * Encoding for AMD64 "XB" bits
     ********************************/
    inline unsigned char XB(Register b) {
      if((b > RSP && b < XMM0) || b > XMM7) {
				return 0x4b;
      }
      
      return 0x4a;
    }
    
    /********************************
     * Encoding for AMD64 "XB" bits
     ********************************/
    inline unsigned char XB32(Register b) {
      if((b > RSP && b < XMM0) || b > XMM7) {
				return 0x66;
      }
      
      return 0x67;
    }
    
    /********************************
     * Encoding for AMD64 "RXB" bits
     ********************************/
    inline unsigned char RXB(Register r, Register b) {
      unsigned char value = 0x4a;
      if((r > RSP && r < XMM0) || r > XMM7) {
				value += 0x4;
      }
      
      if((b > RSP && b < XMM0) || b > XMM7) {
				value += 0x1;
      }
      
      return value;
    }

    /********************************
     * Encoding for AMD64 "RXB" bits
     ********************************/
    inline unsigned char RXB32(Register r, Register b) {
      unsigned char value = 0x42;
      if((r > RSP && r < XMM0) || r > XMM7) {
				value += 0x4;
      }
      
      if((b > RSP && b < XMM0) || b > XMM7) {
				value += 0x1;
      }
      
      return value;
    }
    
    /********************************
     * Encoding for AMD64 "ROB" bits
     ********************************/
    inline unsigned char ROB(Register r, Register b) {
      unsigned char value = 0x48;
      if((r > RSP && r < XMM0) || r > XMM7) {
				value += 0x4;
      }

      if((b > RSP && b < XMM0) || b > XMM7) {
				value += 0x1;
      }
      
      return value;
    }
    
    /********************************
     * Caculates the AMD64 MOD R/M
     * offset
     ********************************/
    inline unsigned char ModRM(Register eff_adr, Register mod_rm) {
      unsigned char byte;

      switch(mod_rm) {
      case RSP:
      case XMM4:
      case R12:
      case XMM12:
				byte = 0xa0;
				break;

      case RAX:
      case XMM0:
      case R8:
      case XMM8:
				byte = 0x80;
				break;

      case RBX:
      case XMM3:
      case R11:
      case XMM11:
				byte = 0x98;
				break;

      case RCX:
      case XMM1:
      case R9:
      case XMM9:
				byte = 0x88;
				break;

      case RDX:
      case XMM2:
      case R10:
      case XMM10:
				byte = 0x90;
				break;

      case RDI:
      case XMM7:
      case R15:
      case XMM15:
				byte = 0xb8;
				break;

      case RSI:
      case XMM6:
      case R14:
      case XMM14:
				byte = 0xb0;
				break;

      case RBP:
      case XMM5:
      case R13:
      case XMM13:
				byte = 0xa8;
				break;

      default:
				wcerr << L"internal error" << endl;
				exit(1);
				break;
      }
      
      switch(eff_adr) {
      case RAX:
      case XMM0:
      case R8:
      case XMM8:
				break;

      case RBX:
      case XMM3:
      case R11:
      case XMM11:
				byte += 3;
				break;

      case RCX:
      case XMM1:
      case R9:
      case XMM9:
				byte += 1;
				break;

      case RDX:
      case XMM2:
      case R10:
      case XMM10:
				byte += 2;
				break;

      case RDI:
      case XMM7:
      case R15:
      case XMM15:
				byte += 7;
				break;

      case RSI:
      case XMM6:
      case R14:
      case XMM14:
				byte += 6;
				break;

      case RBP:
      case XMM5:
      case R13:
      case XMM13:
				byte += 5;
				break;

      case XMM4:
      case R12:
      case XMM12:
				byte += 4;
				break;
	
				// should never happen for esp
      case RSP:
				wcerr << L"invalid register reference" << endl;
				exit(1);
				break;

      default:
				wcerr << L"internal error" << endl;
				exit(1);
				break;
      }
      
      return byte;
    }

    /********************************
     * Returns the name of a register
     ********************************/
    wstring GetRegisterName(Register reg) {
      switch(reg) {
      case RAX:
				return L"rax";

      case RBX:
				return L"rbx";

      case RCX:
				return L"rcx";

      case RDX:
				return L"rdx";

      case RDI:
				return L"rdi";

      case RSI:
				return L"rsi";

      case RBP:
				return L"rbp";

      case RSP:
				return L"rsp";

      case R8:
				return L"r8";

      case R9:
				return L"r9";
	
      case R10:
				return L"r10";

      case R11:
				return L"r11";

      case R12:
				return L"r12";
	
      case R13:
				return L"r13";
	
      case R14:
				return L"r14";
	
      case R15:
				return L"r15";

      case XMM0:
				return L"xmm0";

      case XMM1:
				return L"xmm1";

      case XMM2:
				return L"xmm2";

      case XMM3:
				return L"xmm3";

      case XMM4:
				return L"xmm4";

      case XMM5:
				return L"xmm5";

      case XMM6:
				return L"xmm6";
	
      case XMM7:
				return L"xmm7";
	
      case XMM8:
				return L"xmm8";

      case XMM9:
				return L"xmm9";

      case XMM10:
				return L"xmm10";

      case XMM11:
				return L"xmm11";

      case XMM12:
				return L"xmm12";

      case XMM13:
				return L"xmm13";

      case XMM14:
				return L"xmm14";
	
      case XMM15:
				return L"xmm15";
      }

      return L"?";
    }

    /********************************
     * Encodes a byte array with a
     * 32-bit value
     ********************************/
    inline void ByteEncode32(unsigned char buffer[], int value) {
      memcpy(buffer, &value, sizeof(int));
    }

    /********************************
     * Encodes a byte array with a
     * 64-bit value
     ********************************/
    inline void ByteEncode64(unsigned char buffer[], long value) {
      memcpy(buffer, &value, sizeof(long));
    }
    
    /********************************
     * Encodes an array with the 
     * binary ID of a register
     ********************************/
    inline void RegisterEncode3(unsigned char& code, long offset, Register reg) {
#ifdef _DEBUG
      assert(offset == 2 || offset == 5);
#endif
      
      unsigned char reg_id;
      switch(reg) {
      case RAX:
      case XMM0:
      case R8:
      case XMM8:
				reg_id = 0x0;
				break;

      case RBX:
      case XMM3:
      case R11:
      case XMM11:
				reg_id = 0x3;     
				break;

      case RCX:
      case XMM1:
      case R9:
      case XMM9:
				reg_id = 0x1;
				break;

      case RDX:
      case XMM2:
      case R10:
      case XMM10:
				reg_id = 0x2;
				break;

      case RDI:
      case XMM7:
      case R15:
      case XMM15:
				reg_id = 0x7;
				break;

      case RSI:
      case XMM6:
      case R14:
      case XMM14:
				reg_id = 0x6;
				break;

      case RSP:
      case XMM4:
      case R12:
      case XMM12:
				reg_id = 0x4;
				break;

      case RBP:
      case XMM5:
      case R13:
      case XMM13:
				reg_id = 0x5;
				break;
	
      default:
				wcerr << L"internal error" << endl;
				exit(1);
				break;
      }

      if(offset == 2) {
				reg_id = reg_id << 3;
      }
      code = code | reg_id;
    }
    
    // method.
    RegisterHolder* ArrayIndex(JitInstruction* instr, RuntimeType type) {
      RegInstr* holder = working_stack.front();
      working_stack.pop_front();

      RegisterHolder* array_holder;
      switch(holder->GetType()) {
      case IMM_INT:
        wcerr << L">>> trying to index a constant! <<<" << endl;
        exit(1);
        break;

      case REG_INT:
        array_holder = holder->GetRegister();
        break;

      case MEM_INT:
        array_holder = GetRegister();
        move_mem_reg(holder->GetOperand(), RBP, array_holder->GetRegister());
        break;
	
      default:
        wcerr << L"internal error" << endl;
        exit(1);
        break;
      }
      // CheckNilDereference(array_holder->GetRegister());
      
      /* Algorithm:
         long index = PopInt();
         const long dim = instr->GetOperand();
	
         for(int i = 1; i < dim; i++) {
         index *= array[i];
         index += PopInt();
         }
      */

      /*
      if(holder) {
        delete holder;
        holder = NULL;
      }

      // get initial index
      RegisterHolder* index_holder;
      holder = working_stack.front();
      working_stack.pop_front();
      switch(holder->GetType()) {
      case IMM_INT:
        index_holder = GetRegister();
        move_imm_reg(holder->GetOperand(), index_holder->GetRegister());
        break;

      case REG_INT:
        index_holder = holder->GetRegister();
        break;

      case MEM_INT:
        index_holder = GetRegister();
        move_mem_reg(holder->GetOperand(), RBP, index_holder->GetRegister());
        break;
	
      default:
        wcerr << L"internal error" << endl;
        exit(1);
        break;
      }
      
      const long dim = instr->GetOperand();
      for(int i = 1; i < dim; i++) {
        // index *= array[i];
        mul_mem_reg((i + 2) * sizeof(long), array_holder->GetRegister(), 
                    index_holder->GetRegister());
        if(holder) {
          delete holder;
          holder = NULL;
        }

        holder = working_stack.front();
        working_stack.pop_front();
        switch(holder->GetType()) {
        case IMM_INT:
          add_imm_reg(holder->GetOperand(), index_holder->GetRegister());
          break;

        case REG_INT:
          add_reg_reg(holder->GetRegister()->GetRegister(), 
                      index_holder->GetRegister());
          break;

        case MEM_INT:
          add_mem_reg(holder->GetOperand(), RBP, index_holder->GetRegister());
          break;

        default:
          break;
        }
      }
      
      // bounds check
      RegisterHolder* bounds_holder = GetRegister();
      move_mem_reg(0, array_holder->GetRegister(), bounds_holder->GetRegister()); 
      
      // ajust indices
      switch(type) {
      case BYTE_ARY_TYPE:
        break;

      case CHAR_ARY_TYPE:
        shl_imm_reg(2, index_holder->GetRegister());
        shl_imm_reg(2, bounds_holder->GetRegister());
        break;
	      
      case INT_TYPE:
      case FLOAT_TYPE:
        shl_imm_reg(3, index_holder->GetRegister());
        shl_imm_reg(3, bounds_holder->GetRegister());
        break;
	
      default:
        break;
      }
      CheckArrayBounds(index_holder->GetRegister(), bounds_holder->GetRegister());
      ReleaseRegister(bounds_holder);

      // skip first 2 integers (size and dimension) and all dimension indices
      add_imm_reg((instr->GetOperand() + 2) * sizeof(long), index_holder->GetRegister());
      add_reg_reg(index_holder->GetRegister(), array_holder->GetRegister());
      ReleaseRegister(index_holder);

      delete holder;
      holder = NULL;
      */
      
      return array_holder;
    }

		// Caculates the indices for
    // memory references.
    void ProcessIndices() {
			local_space = -TMP_REG_2;

      // update frame offsets
      for(size_t i = 0; i < block_instrs.size(); i++) {
        JitInstruction* instr = block_instrs[i];
        switch(instr->GetType()) {
				case LOAD_INT_VAR:
				case STOR_INT_VAR:
				case LOAD_FLOAT_VAR:
				case STOR_FLOAT_VAR:
					instr->SetOperand3(instr->GetOperand() * sizeof(Value));
					break;

				default:
					break;
        }
      }
    }
    
    void Prolog();
    void Epilog(long imm);
    void ProcessInstructions();
    void ProcessLoad(JitInstruction* instr);
    void ProcessStore(JitInstruction* instruction);
    void ProcessStoreIntElement(JitInstruction* instruction);
		void ProcessIntToFloat();
		void ProcessFloatToInt();
    void ProcessIntCalculation(JitInstruction* instruction);
    void ProcessFloatCalculation(JitInstruction* instruction);
		void ProcessJump(JitInstruction* instr);
    void ProcessMethodCall(JitInstruction* instr);
    RegInstr* ProcessIntFold(long left_imm, long right_imm, JitInstructionType type);
    
    // Gets an avaiable register from
    // the pool of registers
    RegisterHolder* GetRegister(bool use_aux = true) {
      RegisterHolder* holder;
      if(aval_regs.empty()) {
				if(use_aux && !aux_regs.empty()) {
					holder = aux_regs.top();
					aux_regs.pop();
				}
				else {
					compile_success = false;
#ifdef _DEBUG
					wcout << L">>> No general registers avaiable! <<<" << endl;
#endif
					aux_regs.push(new RegisterHolder(RAX));
					holder = aux_regs.top();
					aux_regs.pop();
				}
      }
      else {
        holder = aval_regs.back();
        aval_regs.pop_back();
        used_regs.push_back(holder);
      }
#ifdef _VERBOSE
      wcout << L"\t * allocating " << GetRegisterName(holder->GetRegister())
						<< L" *" << endl;
#endif

      return holder;
    }

    // Returns a register to the pool
    void ReleaseRegister(RegisterHolder* h) {
#ifdef _VERBOSE
      wcout << L"\t * releasing " << GetRegisterName(h->GetRegister())
						<< L" *" << endl;
#endif

#ifdef _DEBUG
      assert(h->GetRegister() < XMM0);
      for(size_t i  = 0; i < aval_regs.size(); i++) {
				assert(h != aval_regs[i]);
      }
#endif

      if(h->GetRegister() == RDI || h->GetRegister() == RSI) {
				aux_regs.push(h);
      }
      else {
				aval_regs.push_back(h);
				used_regs.remove(h);
      }
    }

    // Gets an avaiable register from
    // the pool of registers
    RegisterHolder* GetXmmRegister() {
      RegisterHolder* holder;
      if(aval_xregs.empty()) {
				compile_success = false;
#ifdef _DEBUG
				wcout << L">>> No XMM registers avaiable! <<<" << endl;
#endif
				aval_xregs.push_back(new RegisterHolder(XMM0));
				holder = aval_xregs.back();
        aval_xregs.pop_back();
        used_xregs.push_back(holder);
      }
      else {
        holder = aval_xregs.back();
        aval_xregs.pop_back();
        used_xregs.push_back(holder);
      }
#ifdef _VERBOSE
      wcout << L"\t * allocating " << GetRegisterName(holder->GetRegister())
						<< L" *" << endl;
#endif

      return holder;
    }

    // Returns a register to the pool
    void ReleaseXmmRegister(RegisterHolder* h) {
#ifdef _DEBUG
      assert(h->GetRegister() >= XMM0);
      for(size_t i = 0; i < aval_xregs.size(); i++) {
				assert(h != aval_xregs[i]);
      }
#endif
      
#ifdef _VERBOSE
      wcout << L"\t * releasing: " << GetRegisterName(h->GetRegister())
						<< L" * " << endl;
#endif
      aval_xregs.push_back(h);
      used_xregs.remove(h);
    }
    
    // move instructions
    void move_reg_mem8(Register src, long offset, Register dest);
    void move_mem8_reg(long offset, Register src, Register dest);
    void move_imm_mem8(long imm, long offset, Register dest);
    void move_reg_mem32(Register src, long offset, Register dest);
    void move_mem32_reg(long offset, Register src, Register dest);
    void move_reg_reg(Register src, Register dest);
    void move_reg_mem(Register src, long offset, Register dest);
    void move_mem_reg(long offset, Register src, Register dest);
    void move_imm_memx(RegInstr* instr, long offset, Register dest);
    void move_imm_mem(long imm, long offset, Register dest);
    void move_imm_reg(long imm, Register reg);
    void move_imm_xreg(RegInstr* instr, Register reg);
    void move_mem_xreg(long offset, Register src, Register dest);
    void move_xreg_mem(Register src, long offset, Register dest);
    void move_xreg_xreg(Register src, Register dest);

    // math instructions
    void math_imm_reg(long imm, Register reg, JitInstructionType type);    
    void math_imm_xreg(RegInstr* instr, Register reg, JitInstructionType type);
    void math_reg_reg(Register src, Register dest, JitInstructionType type);
    void math_xreg_xreg(Register src, Register dest, JitInstructionType type);
    void math_mem_reg(long offset, Register reg, JitInstructionType type);
    void math_mem_xreg(long offset, Register reg, JitInstructionType type);

    // logical
    void and_imm_reg(long imm, Register reg);
    void and_reg_reg(Register src, Register dest);
    void and_mem_reg(long offset, Register src, Register dest);
    void or_imm_reg(long imm, Register reg);
    void or_reg_reg(Register src, Register dest);
    void or_mem_reg(long offset, Register src, Register dest);
    void xor_imm_reg(long imm, Register reg);    
    void xor_reg_reg(Register src, Register dest);
    void xor_mem_reg(long offset, Register src, Register dest);
    
    // add instructions
    void add_imm_mem(long imm, long offset, Register dest);    
    void add_imm_reg(long imm, Register reg);    
    void add_imm_xreg(RegInstr* instr, Register reg);
    void add_xreg_xreg(Register src, Register dest);
    void add_mem_reg(long offset, Register src, Register dest);
    void add_mem_xreg(long offset, Register src, Register dest);
    void add_reg_reg(Register src, Register dest);

    // sub instructions
    void sub_imm_xreg(RegInstr* instr, Register reg);
    void sub_xreg_xreg(Register src, Register dest);
    void sub_mem_xreg(long offset, Register src, Register dest);
    void sub_imm_reg(long imm, Register reg);
    void sub_imm_mem(long imm, long offset, Register dest);
    void sub_reg_reg(Register src, Register dest);
    void sub_mem_reg(long offset, Register src, Register dest);

    // mul instructions
    void mul_imm_xreg(RegInstr* instr, Register reg);
    void mul_xreg_xreg(Register src, Register dest);
    void mul_mem_xreg(long offset, Register src, Register dest);
    void mul_imm_reg(long imm, Register reg);
    void mul_reg_reg(Register src, Register dest);
    void mul_mem_reg(long offset, Register src, Register dest);

    // div instructions
    void div_imm_xreg(RegInstr* instr, Register reg);
    void div_xreg_xreg(Register src, Register dest);
    void div_mem_xreg(long offset, Register src, Register dest);
    void div_imm_reg(long imm, Register reg, bool is_mod = false);
    void div_reg_reg(Register src, Register dest, bool is_mod = false);
    void div_mem_reg(long offset, Register src, Register dest, bool is_mod = false);

    // compare instructions
    void cmp_reg_reg(Register src, Register dest);
    void cmp_mem_reg(long offset, Register src, Register dest);
    void cmp_imm_reg(long imm, Register reg);
    void cmp_xreg_xreg(Register src, Register dest);
    void cmp_mem_xreg(long offset, Register src, Register dest);
    void cmp_imm_xreg(RegInstr* instr, Register reg);
    void cmov_reg(Register reg, JitInstructionType oper);
    
    // inc/dec instructions
    void dec_reg(Register dest);
    void dec_mem(long offset, Register dest);
    void inc_mem(long offset, Register dest);
    
    // shift instructions
    void shl_reg_reg(Register src, Register dest);
    void shl_mem_reg(long offset, Register src, Register dest);
    void shl_imm_reg(long value, Register dest);
    void shr_reg_reg(Register src, Register dest);
    void shr_mem_reg(long offset, Register src, Register dest);
    void shr_imm_reg(long value, Register dest);
    
    // push/pop instructions
    void push_imm(long value);
    void push_reg(Register reg);
    void pop_reg(Register reg);
    void push_mem(long offset, Register src);
    
    // type conversion instructions
    void round_imm_xreg(RegInstr* instr, Register reg, bool is_floor);
    void round_mem_xreg(long offset, Register src, Register dest, bool is_floor);
    void round_xreg_xreg(Register src, Register dest, bool is_floor);
    void cvt_xreg_reg(Register src, Register dest);
    void cvt_imm_reg(RegInstr* instr, Register reg);
    void cvt_mem_reg(long offset, Register src, Register dest);
    void cvt_reg_xreg(Register src, Register dest);
    void cvt_imm_xreg(RegInstr* instr, Register reg);
    void cvt_mem_xreg(long offset, Register src, Register dest);
    
    // function call instruction
    void call_reg(Register reg);

    // generates a conditional jump
    bool cond_jmp(JitInstructionType type);
    
  public: 
    JitCompiler(vector<JitInstruction*> block_instrs, unordered_map<INT_T, size_t>* jump_table, INT_T end_label) {
      this->block_instrs = block_instrs;
			this->jump_table = jump_table;
			this->end_label = end_label;
      skip_jump = false;
    }

    ~JitCompiler() {
			if(code) {
				free(code);
				code = NULL;
			}

			if(floats) {
				delete[] floats;
				floats = NULL;
			}
			
			for(size_t i = 0; i < aval_regs.size(); i++) {
				RegisterHolder* tmp = aval_regs[i];
				delete tmp;
				tmp = NULL;
			}
			aval_regs.clear();

			for(list<RegisterHolder*>::iterator iter = used_regs.begin(); iter != used_regs.end(); ++iter) {
				RegisterHolder* tmp = *iter;
				delete tmp;
				tmp = NULL;
			}
			
			while(!aux_regs.empty()) {
				RegisterHolder* tmp = aux_regs.top();
				delete tmp;
				tmp = NULL;
				aux_regs.pop();
			}

			for(size_t i = 0; i < aval_xregs.size(); i++) {
				RegisterHolder* tmp = aval_xregs[i];
				delete tmp;
				tmp = NULL;
			}
			aval_xregs.clear();
			
			for(list<RegisterHolder*>::iterator iter = used_xregs.begin(); iter != used_xregs.end(); ++iter) {
				RegisterHolder* tmp = *iter;
				delete tmp;
				tmp = NULL;
			}
    }
    
    //
    // Compiles stack code into AMD64 machine code
    //
    jit_fun_ptr Compile() {
      compile_success = true;
      
			// code buffer memory
			code_buf_max = PAGE_SIZE;
			if(posix_memalign((void**)&code, PAGE_SIZE, PAGE_SIZE)) {
				wcerr << L"Unable to reallocate JIT memory!" << endl;
				exit(1);
			}
			if(mprotect(code, code_buf_max, PROT_READ | PROT_WRITE | PROT_EXEC) < 0) {
				wcerr << L"Unable to mprotect" << endl;
				exit(1);
			}
			// floats memory
			if(posix_memalign((void**)&floats, PAGE_SIZE, sizeof(double) * MAX_DBLS)) {
				wcerr << L"Unable to reallocate JIT memory!" << endl;
				exit(1);
			}
			floats_index = instr_index = code_index = instr_count = 0;
			// general use registers
			//	aval_regs.push_back(new RegisterHolder(RDX));
			//	aval_regs.push_back(new RegisterHolder(RCX));
			aval_regs.push_back(new RegisterHolder(RBX));
			aval_regs.push_back(new RegisterHolder(RAX));
			// aux general use registers
			//        aux_regs.push(new RegisterHolder(RDI));
			//        aux_regs.push(new RegisterHolder(RSI));
			aux_regs.push(new RegisterHolder(R15));
			aux_regs.push(new RegisterHolder(R14));
			aux_regs.push(new RegisterHolder(R13));
			// aux_regs.push(new RegisterHolder(R12));
			aux_regs.push(new RegisterHolder(R11));
			aux_regs.push(new RegisterHolder(R10));
			// aux_regs.push(new RegisterHolder(R9));
			aux_regs.push(new RegisterHolder(R8));
			// floating point registers
			aval_xregs.push_back(new RegisterHolder(XMM15));
			aval_xregs.push_back(new RegisterHolder(XMM14)); 
			aval_xregs.push_back(new RegisterHolder(XMM13));
			aval_xregs.push_back(new RegisterHolder(XMM12)); 
			aval_xregs.push_back(new RegisterHolder(XMM11));
			aval_xregs.push_back(new RegisterHolder(XMM10));   
#ifdef _DEBUG
			wcout << L"===========================================" << endl;
      wcout << L"=== Compiling block for AMD64 (LP) host ===" << endl;
      wcout << L"===========================================" << endl;
#endif
	
			// process offsets
			ProcessIndices();
			// setup
			Prolog();
			
			move_reg_mem(RDI, FRAME, RBP);
			
			// tranlsate program
			ProcessInstructions();
      if(!compile_success) {
        return NULL;
      }
      Epilog(-1);
			
			// show content
			unordered_map<long, JitInstruction*>::iterator iter;
			for(iter = native_jump_table.begin(); iter != native_jump_table.end(); ++iter) {
				JitInstruction* instr = iter->second;
				long src_offset = iter->first;
				// long dest_index = labels[instr->GetOperand()]; // jump_table[instr->GetOperand()];
				// long dest_offset = block_instrs[dest_index]->GetOffset();
				long dest_offset = jump_labels[instr->GetOperand()]->GetOffset();
				long offset = dest_offset - src_offset - 4;
				memcpy(&code[src_offset], &offset, 4); 
#ifdef _DEBUG
				wcout << L"jump update: id=" << instr->GetOperand() << L"; src=" << src_offset 
							<< L"; dest=" << dest_offset << endl;
#endif
			}
#ifdef _DEBUG
			wcout << L"JIT code: actual_size=" << code_index << L", buffer_size=" 
						<< code_buf_max << L" byte(s)" << endl;
#endif
			
			if(mprotect(code, code_index, PROT_EXEC | PROT_WRITE)) {
				perror("Couldn't mprotect");
				exit(errno);
			}
			
#ifdef _DEBUG
			wcout << L"--------------------------" << endl;
#endif      
			
      return (jit_fun_ptr)code;
    }
  };
}

#endif
