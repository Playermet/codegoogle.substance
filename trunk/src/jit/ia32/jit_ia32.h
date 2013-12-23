/***************************************************************************
* IA-32 tracing JIT compiler
*
* Copyright (c) 2013, Randy Hollines
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

#ifndef __JIT_IA32_H__
#define __JIT_IA32_H__

#include "../../common.h"
#include "../jit_common.h"
#ifdef _WIN32
#include <stdint.h>
#else
#include <sys/mman.h>
#include <errno.h>
#endif

// offsets for Intel (IA-32) addresses
#define FRAME 8
#define INSTANCE_MEM 12
#define CLASS_MEM 16
// integer temps
#define TMP_REG_0 -8
#define TMP_REG_1 -12
#define TMP_REG_2 -16

#define MAX_DBLS 64
#define PAGE_SIZE 4096
#define VALUE_OFFSET 8

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

  // general and SSE (x86) registers
  typedef enum _Register { 
    EAX = -5000, 
    EBX, 
    ECX, 
    EDX, 
    EDI,
    ESI,
    EBP,
    ESP,
    XMM0, 
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7
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

    void SetOperand(int32_t o) {
      operand = o;
    }

    int32_t GetOperand() {
      return operand;
    }
  };

  /********************************
  * JIT compiler class
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
		unordered_map<int32_t, JitInstruction*> jump_labels;
    unordered_map<int32_t, JitInstruction*> native_jump_table;
    int32_t local_space;
    int32_t instr_count;
    unsigned char* code;
    int32_t code_index;   
    double* floats;     
    int32_t floats_index;
    size_t instr_index;
    int32_t code_buf_max;
    bool compile_success;
    bool skip_jump;
		INT_T end_label;

    // Add byte code to buffer
    inline void AddMachineCode(unsigned char b) {
      if(code_index == code_buf_max) {
  #ifdef _WIN32
        code = (unsigned char*)realloc(code, code_buf_max * 2); 
        if(!code) {
          wcerr << L"Unable to allocate memory!" << std::endl;
          exit(1);
        }
  #else
        unsigned char* tmp;	
        if(posix_memalign((void**)&tmp, PAGE_SIZE, code_buf_max * 2)) {
          wcerr << L"Unable to reallocate JIT memory!" << std::endl;
          exit(1);
        }
        memcpy(tmp, code, code_index);
        free(code);
        code = tmp;
  #endif	
        code_buf_max *= 2;
      }
      code[code_index++] = b;
    }

    // Encodes and writes out a 32-bit integer value
    inline void AddImm(int32_t imm) {
      unsigned char buffer[sizeof(int32_t)];
      ByteEncode32(buffer, imm);
      for(int32_t i = 0; i < (int32_t)sizeof(int32_t); i++) {
        AddMachineCode(buffer[i]);
      }
    }

    // Encodes and writes out a 16-bit integer value
    inline void AddImm16(int16_t imm) {
      unsigned char buffer[sizeof(int16_t)];
      ByteEncode16(buffer, imm);
      for(int16_t i = 0; i < (int16_t)sizeof(int16_t); i++) {
        AddMachineCode(buffer[i]);
      }
    }

    // Caculates the IA-32 MOD R/M
    // offset
    inline unsigned ModRM(Register eff_adr, Register mod_rm) {
      unsigned byte = 0;

      switch(mod_rm) {
      case ESP:
      case XMM4:
        byte = 0xa0;
        break;

      case EAX:
      case XMM0:
        byte = 0x80;
        break;

      case EBX:
      case XMM3:
        byte = 0x98;
        break;

      case ECX:
      case XMM1:
        byte = 0x88;
        break;

      case EDX:
      case XMM2:
        byte = 0x90;
        break;

      case EDI:
      case XMM7:
        byte = 0xb8;
        break;

      case ESI:
      case XMM6:
        byte = 0xb0;
        break;

      case EBP:
      case XMM5:
        byte = 0xa8;
        break;
      }

      switch(eff_adr) {
      case EAX:
      case XMM0:
        break;

      case EBX:
      case XMM3:
        byte += 3;
        break;

      case ECX:
      case XMM1:
        byte += 1;
        break;

      case EDX:
      case XMM2:
        byte += 2;
        break;

      case EDI:
      case XMM7:
        byte += 7;
        break;

      case ESI:
      case XMM6:
        byte += 6;
        break;

      case EBP:
      case XMM5:
        byte += 5;
        break;

      case XMM4:
        byte += 4;
        break;

        // should never happen for esp
      case ESP:
        wcerr << L">>> invalid register reference <<<" << std::endl;
        exit(1);
        break;
      }

      return byte;
    }

    // Returns the name of a register
    wstring GetRegisterName(Register reg) {
      switch(reg) {
      case EAX:
        return L"eax";

      case EBX:
        return L"ebx";

      case ECX:
        return L"ecx";

      case EDX:
        return L"edx";

      case EDI:
        return L"edi";

      case ESI:
        return L"esi";

      case EBP:
        return L"ebp";

      case ESP:
        return L"esp";

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
      }

      return L"unknown";
    }

    // Encodes a byte array with a 32-bit value
    inline void ByteEncode32(unsigned char buffer[], int32_t value) {
      memcpy(buffer, &value, sizeof(int32_t));
    }

    // Encodes a byte array with a 16-bit value
    inline void ByteEncode16(unsigned char buffer[], int16_t value) {
      memcpy(buffer, &value, sizeof(int16_t));
    }

    // Encodes an array with the 
    // binary ID of a register
    inline void RegisterEncode3(unsigned char& code, int32_t offset, Register reg) {
  #ifdef _DEBUG
      assert(offset == 2 || offset == 5);
  #endif

      unsigned char reg_id;
      switch(reg) {
      case EAX:
      case XMM0:
        reg_id = 0x0;
        break;

      case EBX:
      case XMM3:
        reg_id = 0x3;     
        break;

      case ECX:
      case XMM1:
        reg_id = 0x1;
        break;

      case EDX:
      case XMM2:
        reg_id = 0x2;
        break;

      case EDI:
      case XMM7:
        reg_id = 0x7;
        break;

      case ESI:
      case XMM6:
        reg_id = 0x6;
        break;

      case ESP:
      case XMM4:
        reg_id = 0x4;
        break;

      case EBP:
      case XMM5:
        reg_id = 0x5;
        break;

      default:
        wcerr << L"internal error" << std::endl;
        exit(1);
        break;
      }

      if(offset == 2) {
        reg_id = reg_id << 3;
      }
      code = code | reg_id;
    }

    //
    // Resolve array index
    //
    // TODO: bounds checking
    RegisterHolder* ArrayIndex(JitInstruction* instr) {
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
        move_mem_reg(FRAME, EBP, array_holder->GetRegister());
        add_imm_reg(holder->GetOperand() + VALUE_OFFSET, array_holder->GetRegister());
        move_mem_reg(0, array_holder->GetRegister(), array_holder->GetRegister());        
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
        move_mem_reg(FRAME, EBP, index_holder->GetRegister());
        add_imm_reg(holder->GetOperand() + VALUE_OFFSET, index_holder->GetRegister());
        move_mem_reg(0, index_holder->GetRegister(), index_holder->GetRegister());        
        break;
	
      default:
        wcerr << L"internal error" << endl;
        exit(1);
        break;
      }
      
      const long dim = instr->GetOperand();
      for(int i = 1; i < dim; i++) {
        // index *= array[i];
        mul_mem_reg((i + 2) * sizeof(long), array_holder->GetRegister(), index_holder->GetRegister());
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
          add_reg_reg(holder->GetRegister()->GetRegister(), index_holder->GetRegister());
          break;

        case MEM_INT: {
          RegisterHolder* tmp_holder = GetRegister();
          move_mem_reg(FRAME, EBP, tmp_holder->GetRegister());
          add_imm_reg(holder->GetOperand() + VALUE_OFFSET, tmp_holder->GetRegister());
          add_mem_reg(0, tmp_holder->GetRegister(), index_holder->GetRegister()); 
          ReleaseRegister(tmp_holder);
        }
          break;

        default:
          break;
        }
      }
      
      /*
      // bounds check
      RegisterHolder* bounds_holder = GetRegister();
      move_mem_reg(0, array_holder->GetRegister(), bounds_holder->GetRegister());
      */ 
      
      mul_imm_reg(sizeof(Value), index_holder->GetRegister());
      
      /*
      CheckArrayBounds(index_holder->GetRegister(), bounds_holder->GetRegister());
      ReleaseRegister(bounds_holder);
      */
      
      // skip metadata (note extra padding in Windows)
      add_imm_reg((instr->GetOperand() + 2) * sizeof(long) + VALUE_OFFSET, index_holder->GetRegister());
      add_reg_reg(index_holder->GetRegister(), array_holder->GetRegister());
      ReleaseRegister(index_holder);

      delete holder;
      holder = NULL;
      
      return array_holder;
    }

    // Caculates the indices for
    // memory references.
    void ProcessIndices() {
      // allocate space for local variables
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
    void Epilog(int32_t imm);
    void ProcessInstructions();
    void ProcessLoad(JitInstruction* instr);
    void ProcessLoadIntElement(JitInstruction* instruction);
    void ProcessLoadFloatElement(JitInstruction* instruction);
    void ProcessStore(JitInstruction* instruction);
    void ProcessStoreIntElement(JitInstruction* instruction);
    void ProcessStoreFloatElement(JitInstruction* instruction);
		void ProcessIntToFloat();
		void ProcessFloatToInt();
    void ProcessIntCalculation(JitInstruction* instruction);
    void ProcessFloatCalculation(JitInstruction* instruction);
		void ProcessJump(JitInstruction* instr);
    void ProcessTrap(JitInstruction* instr);
    RegInstr* ProcessIntFold(long left_imm, long right_imm, JitInstructionType type);

    /***********************************
    * Gets an avaiable register from
    ***********************************/
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
          std::wcout << L">>> No general registers avaiable! <<<" << std::endl;
  #endif
          aux_regs.push(new RegisterHolder(EAX));
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
      std::wcout << L"\t * allocating " << GetRegisterName(holder->GetRegister())
        << L" *" << std::endl;
  #endif

      return holder;
    }

    // Returns a register to the pool
    void ReleaseRegister(RegisterHolder* h) {
  #ifdef _VERBOSE
      std::wcout << L"\t * releasing " << GetRegisterName(h->GetRegister())
        << L" *" << std::endl;
  #endif

  #ifdef _DEBUG
      assert(h->GetRegister() < XMM0);
      for(size_t i  = 0; i < aval_regs.size(); i++) {
        assert(h != aval_regs[i]);
      }
  #endif

      if(h->GetRegister() == EDI || h->GetRegister() == ESI) {
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
        std::wcout << L">>> No XMM registers avaiable! <<<" << std::endl;
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
      std::wcout << L"\t * allocating " << GetRegisterName(holder->GetRegister())
        << L" *" << std::endl;
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
      std::wcout << L"\t * releasing: " << GetRegisterName(h->GetRegister())
        << L" * " << std::endl;
  #endif
      aval_xregs.push_back(h);
      used_xregs.remove(h);
    }
		
    // move instructions
    void move_reg_mem8(Register src, int32_t offset, Register dest);
    void move_mem8_reg(int32_t offset, Register src, Register dest);
    void move_imm_mem8(int32_t imm, int32_t offset, Register dest);    
    void move_reg_mem16(Register src, int32_t offset, Register dest);
    void move_mem16_reg(int32_t offset, Register src, Register dest);
    void move_imm_mem16(int32_t imm, int32_t offset, Register dest);
    void move_reg_reg(Register src, Register dest);
    void move_reg_mem(Register src, int32_t offset, Register dest);
    void move_mem_reg(int32_t offset, Register src, Register dest);
    void move_imm_memx(RegInstr* instr, int32_t offset, Register dest);
    void move_imm_mem(int32_t imm, int32_t offset, Register dest);
    void move_imm_reg(int32_t imm, Register reg);
    void move_imm_xreg(RegInstr* instr, Register reg);
    void move_mem_xreg(int32_t offset, Register src, Register dest);
    void move_xreg_mem(Register src, int32_t offset, Register dest);
    void move_xreg_xreg(Register src, Register dest);

    // math instructions
    void math_imm_reg(int32_t imm, Register reg, JitInstructionType type);    
    void math_imm_xreg(RegInstr* instr, Register reg, JitInstructionType type);
    void math_reg_reg(Register src, Register dest, JitInstructionType type);
    void math_xreg_xreg(Register src, Register dest, JitInstructionType type);
    void math_mem_reg(int32_t offset, Register reg, JitInstructionType type);
    void math_mem_xreg(int32_t offset, Register reg, JitInstructionType type);

    // logical
    void and_imm_reg(int32_t imm, Register reg);
    void and_reg_reg(Register src, Register dest);
    void and_mem_reg(int32_t offset, Register src, Register dest);
    void or_imm_reg(int32_t imm, Register reg);
    void or_reg_reg(Register src, Register dest);
    void or_mem_reg(int32_t offset, Register src, Register dest);
    void xor_imm_reg(int32_t imm, Register reg);
    void xor_reg_reg(Register src, Register dest);
    void xor_mem_reg(int32_t offset, Register src, Register dest);

    // add instructions
    void add_imm_mem(int32_t imm, int32_t offset, Register dest);    
    void add_imm_reg(int32_t imm, Register reg);    
    void add_imm_xreg(RegInstr* instr, Register reg);
    void add_xreg_xreg(Register src, Register dest);
    void add_mem_reg(int32_t offset, Register src, Register dest);
    void add_mem_xreg(int32_t offset, Register src, Register dest);
    void add_reg_reg(Register src, Register dest);

    // sub instructions
    void sub_imm_xreg(RegInstr* instr, Register reg);
    void sub_xreg_xreg(Register src, Register dest);
    void sub_mem_xreg(int32_t offset, Register src, Register dest);
    void sub_imm_reg(int32_t imm, Register reg);
    void sub_imm_mem(int32_t imm, int32_t offset, Register dest);
    void sub_reg_reg(Register src, Register dest);
    void sub_mem_reg(int32_t offset, Register src, Register dest);

    // mul instructions
    void mul_imm_xreg(RegInstr* instr, Register reg);
    void mul_xreg_xreg(Register src, Register dest);
    void mul_mem_xreg(int32_t offset, Register src, Register dest);
    void mul_imm_reg(int32_t imm, Register reg);
    void mul_reg_reg(Register src, Register dest);
    void mul_mem_reg(int32_t offset, Register src, Register dest);

    // div instructions
    void div_imm_xreg(RegInstr* instr, Register reg);
    void div_xreg_xreg(Register src, Register dest);
    void div_mem_xreg(int32_t offset, Register src, Register dest);
    void div_imm_reg(int32_t imm, Register reg, bool is_mod = false);
    void div_reg_reg(Register src, Register dest, bool is_mod = false);
    void div_mem_reg(int32_t offset, Register src, Register dest, bool is_mod = false);

    // compare instructions
    void cmp_reg_reg(Register src, Register dest);
    void cmp_mem_reg(int32_t offset, Register src, Register dest);
    void cmp_imm_reg(int32_t imm, Register reg);
    void cmp_xreg_xreg(Register src, Register dest);
    void cmp_mem_xreg(int32_t offset, Register src, Register dest);
    void cmp_imm_xreg(RegInstr* instr, Register reg);
    void cmov_reg(Register reg, JitInstructionType oper);

    // inc/dec instructions
    void dec_reg(Register dest);
    void dec_mem(int32_t offset, Register dest);
    void inc_mem(int32_t offset, Register dest);

    // shift instructions
    void shl_reg_reg(Register src, Register dest);
    void shl_mem_reg(int32_t offset, Register src, Register dest);
    void shl_imm_reg(int32_t value, Register dest);

    void shr_reg_reg(Register src, Register dest);
    void shr_mem_reg(int32_t offset, Register src, Register dest);
    void shr_imm_reg(int32_t value, Register dest);

    // push/pop instructions
    void push_imm(int32_t value);
    void push_reg(Register reg);
    void pop_reg(Register reg);
    void push_mem(int32_t offset, Register src);

    // type conversion instructions
    void round_imm_xreg(RegInstr* instr, Register reg, bool is_floor);
    void round_mem_xreg(int32_t offset, Register src, Register dest, bool is_floor);
    void round_xreg_xreg(Register src, Register dest, bool is_floor);
    void cvt_xreg_reg(Register src, Register dest);
    void cvt_imm_reg(RegInstr* instr, Register reg);
    void cvt_mem_reg(int32_t offset, Register src, Register dest);
    void cvt_reg_xreg(Register src, Register dest);
    void cvt_imm_xreg(RegInstr* instr, Register reg);
    void cvt_mem_xreg(int32_t offset, Register src, Register dest);

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
			/*
			if(code) {
				free(code);
				code = NULL;
			}

			if(floats) {
				delete[] floats;
				floats = NULL;
			}
			*/
			
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

		// Compiles byte code into machine code
		jit_fun_ptr Compile() {
      compile_success = false;
      code_buf_max = PAGE_SIZE;
#ifdef _WIN32
      code = (unsigned char*)malloc(code_buf_max);
      floats = new double[MAX_DBLS];
#else
      if(posix_memalign((void**)&code, PAGE_SIZE, code_buf_max)) {
        wcerr << L"Unable to allocate JIT memory!" << endl;
        exit(1);
      }

      if(posix_memalign((void**)&floats, PAGE_SIZE, sizeof(double) * MAX_DBLS)) {
        wcerr << L"Unable to allocate JIT memory!" << endl;
        exit(1);
      }
#endif

      floats_index = instr_index = code_index = instr_count = 0;
      // general use registers
      aval_regs.push_back(new RegisterHolder(EDX));
      aval_regs.push_back(new RegisterHolder(ECX));
      aval_regs.push_back(new RegisterHolder(EBX));
      aval_regs.push_back(new RegisterHolder(EAX));
      // aux general use registers
      aux_regs.push(new RegisterHolder(EDI));
      aux_regs.push(new RegisterHolder(ESI));
      // floating point registers
      aval_xregs.push_back(new RegisterHolder(XMM7));
      aval_xregs.push_back(new RegisterHolder(XMM6));
      aval_xregs.push_back(new RegisterHolder(XMM5));
      aval_xregs.push_back(new RegisterHolder(XMM4)); 
      aval_xregs.push_back(new RegisterHolder(XMM3));
      aval_xregs.push_back(new RegisterHolder(XMM2)); 
      aval_xregs.push_back(new RegisterHolder(XMM1));
      aval_xregs.push_back(new RegisterHolder(XMM0));   
#ifdef _DEBUG
			wcout << L"========================================" << endl;
      wcout << L"==== Compiling block for IA-32 host ====" << endl;
			wcout << L"========================================" << endl;
#endif
      // TODO: map referenced variables to stack references; impact memory manager
      ProcessIndices();
      Prolog();
      ProcessInstructions();
      if(!compile_success) {
        return NULL;
      }
			Epilog(-1);

			// show content
			unordered_map<int32_t, JitInstruction*>::iterator iter;
			for(iter = native_jump_table.begin(); iter != native_jump_table.end(); ++iter) {
				JitInstruction* instr = iter->second;
				int32_t src_offset = iter->first;
				int32_t dest_offset = jump_labels[instr->GetOperand()]->GetOffset();
				int32_t offset = dest_offset - src_offset - 4;
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

#ifndef _WIN32
			if(mprotect(code, code_index, PROT_EXEC | PROT_WRITE)) {
				perror("Couldn't mprotect");
				exit(errno);
			}
#endif

#ifdef _DEBUG
			wcout << L"--------------------------" << endl;
#endif
			
      return (jit_fun_ptr)code;
    }
  };
}

#endif
