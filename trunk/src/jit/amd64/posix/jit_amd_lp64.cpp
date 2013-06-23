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
 * notice, this lis  of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in 
 * the documentation and/or other materials provided with the distribution.
 * - Neither the name of the Objeck Team nor the names of its 
 * contributors may be used to endorse or promote products derived 
 * from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IPLIED WARRANTIES, INCLUDING, BUT NOT 
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

#include "jit_amd_lp64.h"
#include <string>

using namespace Runtime;

void JitCompiler::Prolog() {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [<prolog>]" << endl;
#endif

  local_space += 16;
  while(local_space % 16 != 0) {
    local_space++;
  }
  local_space+=8;
  
  unsigned char buffer[4];
  ByteEncode32(buffer, local_space);

  unsigned char setup_code[] = {
    // setup stack frame
    0x48, 0x55,                                    // push %rbp
    0x48, 0x89, 0xe5,                              // mov  %rsp, %rbp    
    0x48, 0x81, 0xec,                              // sub  $imm, %rsp
    buffer[0], buffer[1], buffer[2], buffer[3],      
    // save registers
    0x48, 0x53,                                    // push rbx
    /****/
    0x48, 0x51,                                    // push rcx
    0x48, 0x52,                                    // push rdx
    0x48, 0x57,                                    // push rdi
    0x48, 0x56,                                    // push rsi
    0x49, 0x50,                                    // push r8
    0x49, 0x51,                                    // push r9
    /****/
    0x49, 0x52,                                    // push r10
    0x49, 0x53,                                    // push r11
    0x49, 0x54,                                    // push r12
    0x49, 0x55,                                    // push r13
    0x49, 0x56,                                    // push r14
    0x49, 0x57,                                    // push r15
  };
  const long setup_size = sizeof(setup_code);
  // copy setup
  for(long i = 0; i < setup_size; i++) {
    AddMachineCode(setup_code[i]);
  }
}

void JitCompiler::Epilog(long imm) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [<epilog>]" << endl;
#endif
  
  move_imm_reg(imm, RAX);
  
  unsigned char teardown_code[] = {
    // restore registers
    0x49, 0x5f,       // pop r15
    0x49, 0x5e,       // pop r14
    0x49, 0x5d,       // pop r13
    0x49, 0x5c,       // pop r12
    0x49, 0x5b,       // pop r11
    0x49, 0x5a,       // pop r10
    /****/
    0x49, 0x59,       // pop r9
    0x49, 0x58,       // pop r8
    0x48, 0x5e,       // pop rsi
    0x48, 0x5f,       // pop rdi
    0x48, 0x5a,       // pop rdx
    0x48, 0x59,       // pop rcx
    /****/
    0x48, 0x5b,       // pop rbx
    // 0x48, 0x58,       // pop rax    
    // tear down stack frame and return
    0x48, 0x89, 0xec, // mov  %rbp, %rsp
    0x48, 0x5d,       // pop %rbp
    // 0xc9 // leave
    0x48, 0xc3        // rtn
  };
  const long teardown_size = sizeof(teardown_code);
  // copy teardown
  for(long i = 0; i < teardown_size; i++) {
    AddMachineCode(teardown_code[i]);
  }
}

void JitCompiler::ProcessInstructions() {
  for(instr_index = 0; instr_index < block_instrs.size(); instr_index++) {
    JitInstruction* instr = block_instrs[instr_index];
		instr->SetOffset(code_index);
		
    switch(instr->GetType()) {
      // load literal
    case LOAD_CHAR_LIT:
    case LOAD_INT_LIT:
#ifdef _DEBUG
      std::wcout << L"LOAD_INT: value=" << instr->GetOperand() 
								 << L"; regs=" << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      working_stack.push_front(new RegInstr(instr));
      break;

      // float literal
    case LOAD_FLOAT_LIT:
#ifdef _DEBUG
      std::wcout << L"LOAD_FLOAT_LIT: value=" << instr->GetFloatOperand() 
								 << L"; regs=" << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      floats[floats_index] = instr->GetFloatOperand();
      working_stack.push_front(new RegInstr(instr, &floats[floats_index++]));
      break;
      
      // load self
    case LOAD_INST_MEM: {
#ifdef _DEBUG
      std::wcout << L"LOAD_INST_MEM; regs=" << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      working_stack.push_front(new RegInstr(instr));
    }
      break;

      // load self
    case LOAD_CLS_MEM: {
#ifdef _DEBUG
      std::wcout << L"LOAD_CLS_MEM; regs=" << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      working_stack.push_front(new RegInstr(instr));
    }
      break;
      
      // load variable
    case LOAD_INT_VAR:
    case LOAD_FLOAT_VAR:
#ifdef _DEBUG
			if(instr->GetType() == LOAD_INT_VAR) {
				std::wcout << L"LOAD_INT_VAR: ";
			}
			else {
				std::wcout << L"LOAD_FLOAT_VAR: ";
			}			
      std::wcout << L"id=" << instr->GetOperand() << L"; regs="  << aval_regs.size() 
								 << L"," << aux_regs.size() << std::endl;
#endif
      ProcessLoad(instr);
      break;
    
      // store value
    case STOR_INT_VAR:
    case STOR_FLOAT_VAR:
#ifdef _DEBUG
			if(instr->GetType() == STOR_INT_VAR) {
				std::wcout << L"STOR_INT_VAR: ";
			}
			else {
				std::wcout << L"STOR_FLOAT_VAR: ";
			}
      std::wcout << L"id=" << instr->GetOperand() << L"; regs=" 
								 << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      ProcessStore(instr);
      break;
			
      // mathematical
    case AND_INT:
    case OR_INT:
    case ADD_INT:
    case SUB_INT:
    case MUL_INT:
    case DIV_INT:
    case MOD_INT:
    case BIT_AND_INT:
    case BIT_OR_INT:
    case BIT_XOR_INT:
      // comparison
    case LES_INT:
    case GTR_INT:
    case LES_EQL_INT:
    case GTR_EQL_INT:
    case EQL_INT:
    case NEQL_INT:
    case SHL_INT:
    case SHR_INT:
#ifdef _DEBUG
			switch(instr->GetType()) {
			case AND_INT:
				std::wcout << L"AND_INT: ";
				break;
			case OR_INT:
				std::wcout << L"OR_INT: ";
				break;
			case ADD_INT:
				std::wcout << L"ADD_INT: ";
				break;
			case SUB_INT:
				std::wcout << L"SUB_INT: ";
				break;
			case MUL_INT:
				std::wcout << L"MUL_INT: ";
				break;
			case DIV_INT:
				std::wcout << L"DIV_INT: ";
				break;
			case MOD_INT:
				std::wcout << L"MOD_INT: ";
				break;
			case BIT_AND_INT:
				std::wcout << L"BIT_AND_INT: ";
				break;
			case BIT_OR_INT:
				std::wcout << L"BIT_OR_INT: ";
				break;
			case BIT_XOR_INT:
				std::wcout << L"BIT_XOR_INT: ";
				break;
				// comparison
			case LES_INT:
				std::wcout << L"LES_INT: ";
				break;
			case GTR_INT:
				std::wcout << L"GTR_INT: ";
				break;
			case LES_EQL_INT:
				std::wcout << L"LES_EQL_INT: ";
				break;
			case GTR_EQL_INT:
				std::wcout << L"GTR_EQL_INT: ";
				break;
			case EQL_INT:
				std::wcout << L"EQL_INT: ";
				break;
			case NEQL_INT:
				std::wcout << L"NEQL_INT: ";
				break;
			case SHL_INT:
				std::wcout << L"SHL_INT: ";
				break;
			case SHR_INT:
				std::wcout << L"SHR_INT: ";
				break;

			default:
				break;
			}
      std::wcout << L"regs=" << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      ProcessIntCalculation(instr);
      break;
			
    case ADD_FLOAT:
    case SUB_FLOAT:
    case MUL_FLOAT:
    case DIV_FLOAT:
#ifdef _DEBUG
			switch(instr->GetType()) {
			case ADD_FLOAT:
				std::wcout << L"ADD_FLOAT: ";
				break;
			case SUB_FLOAT:
				std::wcout << L"SUB_FLOAT: ";
				break;
			case MUL_FLOAT:
				std::wcout << L"MUL_FLOAT: ";
				break;
			case DIV_FLOAT:
				std::wcout << L"DIV_FLOAT: ";
				break;
			default:
				break;
			}
      std::wcout << L"regs=" << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      ProcessFloatCalculation(instr);
      break;
			
		case LES_FLOAT:
    case GTR_FLOAT:
    case LES_EQL_FLOAT:
    case GTR_EQL_FLOAT:
    case EQL_FLOAT:
    case NEQL_FLOAT: {
#ifdef _DEBUG
			switch(instr->GetType()) {
			case LES_FLOAT:
				std::wcout << L"LES_FLOAT: ";
				break;
			case GTR_FLOAT:
				std::wcout << L"GTR_FLOAT: ";
				break;
			case LES_EQL_FLOAT:
				std::wcout << L"LES_EQL_FLOAT: ";
				break;
			case GTR_EQL_FLOAT:
				std::wcout << L"GTR_EQL_FLOAT: ";
				break;
			case EQL_FLOAT:
				std::wcout << L"EQL_FLOAT: ";
				break;
			case NEQL_FLOAT:
				std::wcout << L"NEQL_FLOAT: ";
				break;
			default:
				break;
			}
			
      std::wcout << L"regs=" << aval_regs.size() << L"," << aux_regs.size() << std::endl;
#endif
      ProcessFloatCalculation(instr);

      RegInstr* left = working_stack.front();
      working_stack.pop_front(); // pop invalid xmm register
      ReleaseXmmRegister(left->GetRegister());

			delete left; 
			left = NULL;
      
			if(!skip_jump) {
				RegisterHolder* holder = GetRegister();
				cmov_reg(holder->GetRegister(), instr->GetType());
				working_stack.push_front(new RegInstr(holder));
			}
    }
      break;
			
		case JMP:
			ProcessJump(instr);
      break;

		case LBL:
#ifdef _DEBUG
			wcout << L"LBL: id=" << instr->GetOperand() << endl;
#endif
			jump_labels[instr->GetOperand()] = instr;
			break;
			
			// TODO:
		default:
			break;
    }
  }

  compile_success = true;
}

void JitCompiler::ProcessLoad(StackInstr* instr) {
  // method/function memory
  if(instr->GetOperand2() == LOCL) {
    if(instr->GetType() == LOAD_FUNC_VAR) {
      RegisterHolder* holder = GetRegister();
      move_mem_reg(instr->GetOperand3() + sizeof(long), RBP, holder->GetRegister());
      working_stack.push_front(new RegInstr(holder));
      
      RegisterHolder* holder2 = GetRegister();
      move_mem_reg(instr->GetOperand3(), RBP, holder2->GetRegister());
      working_stack.push_front(new RegInstr(holder2));
    }
    else {
      working_stack.push_front(new RegInstr(instr));
    }
  }
  // class or instance memory
  else {
    RegInstr* left = working_stack.front();
    working_stack.pop_front();

    RegisterHolder* holder;
    if(left->GetType() == REG_INT) {
      holder = left->GetRegister();
    }
    else {
      holder = GetRegister();
      move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
    }
    // CheckNilDereference(holder->GetRegister());

    // long value
    if(instr->GetType() == LOAD_LOCL_INT_VAR ||
       instr->GetType() == LOAD_CLS_INST_INT_VAR) {
      move_mem_reg(instr->GetOperand3(), holder->GetRegister(), holder->GetRegister());
      working_stack.push_front(new RegInstr(holder));
    }
    // function value
    else if(instr->GetType() == LOAD_FUNC_VAR) {
      RegisterHolder* holder2 = GetRegister();
      move_mem_reg(instr->GetOperand3() + sizeof(long), holder->GetRegister(), holder2->GetRegister());
      working_stack.push_front(new RegInstr(holder2));
      
      move_mem_reg(instr->GetOperand3(), holder->GetRegister(), holder->GetRegister());
      working_stack.push_front(new RegInstr(holder));
    }
    // float value
    else {
      RegisterHolder* xmm_holder = GetXmmRegister();
      move_mem_xreg(instr->GetOperand3(), holder->GetRegister(), xmm_holder->GetRegister());
      ReleaseRegister(holder);
      working_stack.push_front(new RegInstr(xmm_holder));	  
    }

    delete left;
    left = NULL;
  }
}

void JitCompiler::ProcessJump(StackInstr* instr) {
  if(!skip_jump) {
#ifdef _DEBUG
    wcout << L"JMP: id=" << instr->GetOperand() << L", regs=" << aval_regs.size() 
	  << L"," << aux_regs.size() << endl;
#endif
    if(instr->GetOperand2() < 0) {
      AddMachineCode(0xe9);
    }
    else {
      RegInstr* left = working_stack.front();
      working_stack.pop_front(); 

      switch(left->GetType()) {
      case IMM_INT:{
        RegisterHolder* holder = GetRegister();
        move_imm_reg(left->GetOperand(), holder->GetRegister());
        cmp_imm_reg(instr->GetOperand2(), holder->GetRegister());
        ReleaseRegister(holder);
      }
        break;
        
      case REG_INT:
        cmp_imm_reg(instr->GetOperand2(), left->GetRegister()->GetRegister());
        ReleaseRegister(left->GetRegister());
        break;

      case MEM_INT: {
        RegisterHolder* holder = GetRegister();
        move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
        cmp_imm_reg(instr->GetOperand2(), holder->GetRegister());
        ReleaseRegister(holder);
      }
        break;

      default:
        cerr << L">>> Should never occur (compiler bug?) type=" << left->GetType() << L" <<<" << endl;
        exit(1);
        break;
      }

      // 1 byte compare with register
      AddMachineCode(0x0f);
      AddMachineCode(0x84);
      
      // clean up
      delete left;
      left = NULL;
    }
    // store update index
    jump_table.insert(pair<long, StackInstr*>(code_index, instr));
    // temp offset, updated in next pass
    AddImm(0);
  }
  else {
    RegInstr* left = working_stack.front();
    working_stack.pop_front(); 
    skip_jump = false;

    // release register
    if(left->GetType() == REG_INT) {
      ReleaseRegister(left->GetRegister());
    }

    // clean up
    delete left;
    left = NULL;
  }
}

void JitCompiler::ProcessReturnParameters(MemoryType type) {
  switch(type) {
  case INT_TYPE:
    ProcessIntCallParameter();
    break;
    
  case FLOAT_TYPE:
    ProcessFloatCallParameter();
    break;
    
  case FUNC_TYPE:
    ProcessFunctionCallParameter();
    break;

  default:
    break;
  }
}

void JitCompiler::ProcessLoadByteElement(StackInstr* instr) {
  RegisterHolder* elem_holder = ArrayIndex(instr, BYTE_ARY_TYPE);
  RegisterHolder* holder = GetRegister();
  xor_reg_reg(holder->GetRegister(), holder->GetRegister());
  move_mem8_reg(0, elem_holder->GetRegister(), holder->GetRegister());
  ReleaseRegister(elem_holder);
  working_stack.push_front(new RegInstr(holder));
}

void JitCompiler::ProcessLoadCharElement(StackInstr* instr) {
  RegisterHolder* holder = GetRegister();
  RegisterHolder* elem_holder = ArrayIndex(instr, CHAR_ARY_TYPE);
  xor_reg_reg(holder->GetRegister(), holder->GetRegister());
  move_mem32_reg(0, elem_holder->GetRegister(), holder->GetRegister());
  ReleaseRegister(elem_holder);
  working_stack.push_front(new RegInstr(holder));
}

void JitCompiler::ProcessLoadIntElement(StackInstr* instr) {
  RegisterHolder* elem_holder = ArrayIndex(instr, INT_TYPE);
  move_mem_reg(0, elem_holder->GetRegister(), elem_holder->GetRegister());
  working_stack.push_front(new RegInstr(elem_holder));
}

void JitCompiler::ProcessLoadFloatElement(StackInstr* instr) {
  RegisterHolder* elem_holder = ArrayIndex(instr, FLOAT_TYPE);
  RegisterHolder* holder = GetXmmRegister();
  move_mem_xreg(0, elem_holder->GetRegister(), holder->GetRegister());
  working_stack.push_front(new RegInstr(holder));
  ReleaseRegister(elem_holder);
}

void JitCompiler::ProcessStoreByteElement(StackInstr* instr) {
  RegisterHolder* elem_holder = ArrayIndex(instr, BYTE_ARY_TYPE);
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
  case IMM_INT:
    move_imm_mem8(left->GetOperand(), 0, elem_holder->GetRegister());
    ReleaseRegister(elem_holder);
    break;

  case MEM_INT: {    
    // movb can only use al, bl, cl and dl registers
    RegisterHolder* holder = GetRegister(false);
    move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
    move_reg_mem8(holder->GetRegister(), 0, elem_holder->GetRegister());
    ReleaseRegister(holder);
    ReleaseRegister(elem_holder);
  }
    break;

  case REG_INT: {
    // movb can only use al, bl, cl and dl registers
    RegisterHolder* holder = left->GetRegister();
    if(holder->GetRegister() == RDI || holder->GetRegister() == RSI) {
      RegisterHolder* tmp_holder = GetRegister(false);
      move_reg_reg(holder->GetRegister(), tmp_holder->GetRegister());
      move_reg_mem8(tmp_holder->GetRegister(), 0, elem_holder->GetRegister());      
      ReleaseRegister(tmp_holder);
    }
    else {
      move_reg_mem8(holder->GetRegister(), 0, elem_holder->GetRegister());      
    }
    ReleaseRegister(holder);
    ReleaseRegister(elem_holder);
  }
    break;

  default:
    break;
  }
  
  delete left;
  left = NULL;
}

void JitCompiler::ProcessStoreCharElement(StackInstr* instr) {
  RegisterHolder* elem_holder = ArrayIndex(instr, CHAR_ARY_TYPE);
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
  case IMM_INT:
    if(elem_holder->GetRegister() > RSP) {    
      RegisterHolder* holder = GetRegister(false);
      move_reg_reg(elem_holder->GetRegister(), holder->GetRegister());
      ReleaseRegister(elem_holder);
      move_imm_mem(left->GetOperand(), 0, holder->GetRegister());
      ReleaseRegister(holder);
    }
    else {
      move_imm_mem(left->GetOperand(), 0, elem_holder->GetRegister());
      ReleaseRegister(elem_holder);
    }
    break;

  case MEM_INT: {    
    // movb can only use al, bl, cl and dl registers
    RegisterHolder* holder = GetRegister(false);
    move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
    move_reg_mem32(holder->GetRegister(), 0, elem_holder->GetRegister());
    ReleaseRegister(holder);
    ReleaseRegister(elem_holder);
  }
    break;

  case REG_INT: {
    // movb can only use al, bl, cl and dl registers
    RegisterHolder* holder = left->GetRegister();
    if(holder->GetRegister() == RDI || holder->GetRegister() == RSI) {
      RegisterHolder* tmp_holder = GetRegister(false);
      move_reg_reg(holder->GetRegister(), tmp_holder->GetRegister());
      move_reg_mem32(tmp_holder->GetRegister(), 0, elem_holder->GetRegister());      
      ReleaseRegister(tmp_holder);
    }
    else {
      move_reg_mem32(holder->GetRegister(), 0, elem_holder->GetRegister());   
    }
    ReleaseRegister(holder);
    ReleaseRegister(elem_holder);
  }
    break;

  default:
    break;
  }
  
  delete left;
  left = NULL;
}

void JitCompiler::ProcessStoreIntElement(StackInstr* instr) {
  RegisterHolder* elem_holder = ArrayIndex(instr, INT_TYPE);
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
  case IMM_INT:
    move_imm_mem(left->GetOperand(), 0, elem_holder->GetRegister());
    break;

  case MEM_INT: {
    RegisterHolder* holder = GetRegister();
    move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
    move_reg_mem(holder->GetRegister(), 0, elem_holder->GetRegister());
    ReleaseRegister(holder);
  }
    break;

  case REG_INT: {
    RegisterHolder* holder = left->GetRegister();
    move_reg_mem(holder->GetRegister(), 0, elem_holder->GetRegister());
    ReleaseRegister(holder);
  }
    break;

  default:
    break;
  }
  ReleaseRegister(elem_holder);
  
  delete left;
  left = NULL;
}

void JitCompiler::ProcessStoreFloatElement(StackInstr* instr) {
  RegisterHolder* elem_holder = ArrayIndex(instr, FLOAT_TYPE);
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
  case IMM_FLOAT:
    move_imm_memx(left, 0, elem_holder->GetRegister());
    break;

  case MEM_FLOAT: 
  case MEM_INT: {
    RegisterHolder* holder = GetXmmRegister();
    move_mem_xreg(left->GetOperand(), 
		  RBP, holder->GetRegister());
    move_xreg_mem(holder->GetRegister(), 0, elem_holder->GetRegister());
    ReleaseXmmRegister(holder);
  }
    break;

  case REG_FLOAT: {
    RegisterHolder* holder = left->GetRegister();
    move_xreg_mem(holder->GetRegister(), 0, elem_holder->GetRegister());
    ReleaseXmmRegister(holder);
  }
    break;

  default:
    break;
  }
  ReleaseRegister(elem_holder);
  
  delete left;
  left = NULL;
}

void JitCompiler::ProcessFloor(StackInstr* instr) {
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
  case IMM_FLOAT: {
    RegisterHolder* holder = GetXmmRegister();
    round_imm_xreg(left, holder->GetRegister(), true);
    working_stack.push_front(new RegInstr(holder));
    delete left;
    left = NULL;
  }
    break;
    
  case MEM_FLOAT:
  case MEM_INT: {
    RegisterHolder* holder = GetXmmRegister();
    round_mem_xreg(left->GetOperand(), RBP, holder->GetRegister(), true);
    working_stack.push_front(new RegInstr(holder));
    delete left;
    left = NULL;
  }
    break;
    
  case REG_FLOAT:
    round_xreg_xreg(left->GetRegister()->GetRegister(), 
		    left->GetRegister()->GetRegister(), true);
    working_stack.push_front(left);
    break;

  default:
    break;
  }
}

void JitCompiler::ProcessCeiling(StackInstr* instr) {
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
  case IMM_FLOAT: {
    RegisterHolder* holder = GetXmmRegister();
    round_imm_xreg(left, holder->GetRegister(), false);
    working_stack.push_front(new RegInstr(holder));
    delete left;
    left = NULL;
  }
    break;
    
  case MEM_FLOAT:
  case MEM_INT: {
    RegisterHolder* holder = GetXmmRegister();
    round_mem_xreg(left->GetOperand(), RBP, holder->GetRegister(), false);
    working_stack.push_front(new RegInstr(holder));
    delete left;
    left = NULL;
  }
    break;
    
  case REG_FLOAT:
    round_xreg_xreg(left->GetRegister()->GetRegister(), 
		    left->GetRegister()->GetRegister(), false);
    working_stack.push_front(left);
    break;

  default:
    break;
  }
}

void JitCompiler::ProcessFloatToInt(StackInstr* instr) {
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  RegisterHolder* holder = GetRegister();
  switch(left->GetType()) {
  case IMM_FLOAT:
    cvt_imm_reg(left, holder->GetRegister());
    break;
    
  case MEM_FLOAT:
  case MEM_INT:
    cvt_mem_reg(left->GetOperand(), 
		RBP, holder->GetRegister());
    break;

  case REG_FLOAT:
    cvt_xreg_reg(left->GetRegister()->GetRegister(), 
		 holder->GetRegister());
    ReleaseXmmRegister(left->GetRegister());
    break;

  default:
    break;
  }
  working_stack.push_front(new RegInstr(holder));

  delete left;
  left = NULL;
}

void JitCompiler::ProcessIntToFloat(StackInstr* instr) {
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  RegisterHolder* holder = GetXmmRegister();
  switch(left->GetType()) {
  case IMM_INT:
    cvt_imm_xreg(left, holder->GetRegister());
    break;
    
  case MEM_INT:
    cvt_mem_xreg(left->GetOperand(), 
		 RBP, holder->GetRegister());
    break;

  case REG_INT:
    cvt_reg_xreg(left->GetRegister()->GetRegister(), 
		 holder->GetRegister());
    ReleaseRegister(left->GetRegister());
    break;

  default:
    break;
  }
  working_stack.push_front(new RegInstr(holder));

  delete left;
  left = NULL;
}

void JitCompiler::ProcessStore(StackInstr* instr) {
  Register dest;
  RegisterHolder* addr_holder = NULL;

  // instance/method memory
  if(instr->GetOperand2() == LOCL) {
    dest = RBP;
  }
  // class or instance memory
  else {
    RegInstr* left = working_stack.front();
    working_stack.pop_front();
    
    if(left->GetRegister()) {
      addr_holder = left->GetRegister();
    }
    else {
      addr_holder = GetRegister();
      move_mem_reg(left->GetOperand(), RBP, addr_holder->GetRegister());
    }
    dest = addr_holder->GetRegister();
    // CheckNilDereference(dest);
    
    delete left;
    left = NULL;
  }
  
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
  case IMM_INT:
    if(instr->GetType() == STOR_FUNC_VAR) {
      move_imm_mem(left->GetOperand(), instr->GetOperand3(), dest);
      
      RegInstr* left2 = working_stack.front();
      working_stack.pop_front();
      move_imm_mem(left2->GetOperand(), instr->GetOperand3() + sizeof(long), dest);

      delete left2;
      left2 = NULL;
    }
    else {
      move_imm_mem(left->GetOperand(), instr->GetOperand3(), dest);
    }
    break;

  case MEM_INT: {
    RegisterHolder* holder = GetRegister();
    if(instr->GetType() == STOR_FUNC_VAR) {
      move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
      move_reg_mem(holder->GetRegister(), instr->GetOperand3(), dest);

      RegInstr* left2 = working_stack.front();
      working_stack.pop_front();
      move_mem_reg(left2->GetOperand(), RBP, holder->GetRegister());
      move_reg_mem(holder->GetRegister(), instr->GetOperand3() + sizeof(long), dest);

      delete left2;
      left2 = NULL;
    }
    else {      
      move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());            
      move_reg_mem(holder->GetRegister(), instr->GetOperand3(), dest);
    }
    ReleaseRegister(holder);
  }
    break;
    
  case REG_INT: {
    RegisterHolder* holder = left->GetRegister();
    if(instr->GetType() == STOR_FUNC_VAR) {
      move_reg_mem(holder->GetRegister(), instr->GetOperand3(), dest);
      
      RegInstr* left2 = working_stack.front();
      working_stack.pop_front();
      RegisterHolder* holder2  = left2->GetRegister();
      
      move_reg_mem(holder2->GetRegister(), instr->GetOperand3() + sizeof(long), dest);
      ReleaseRegister(holder2);

      delete left2;
      left2 = NULL;
    }
    else {      
      move_reg_mem(holder->GetRegister(), instr->GetOperand3(), dest);
    }
    ReleaseRegister(holder);
  }
    break;

  case IMM_FLOAT:
    move_imm_memx(left, instr->GetOperand3(), dest);
    break;
    
  case MEM_FLOAT: {
    RegisterHolder* holder = GetXmmRegister();
    move_mem_xreg(left->GetOperand(), RBP, holder->GetRegister());
    move_xreg_mem(holder->GetRegister(), instr->GetOperand3(), dest);
    ReleaseXmmRegister(holder);
  }
    break;
    
  case REG_FLOAT: {
    RegisterHolder* holder = left->GetRegister();
    move_xreg_mem(holder->GetRegister(), instr->GetOperand3(), dest);
    ReleaseXmmRegister(holder);
  }
    break;
  }

  if(addr_holder) {
    ReleaseRegister(addr_holder);
  }

  delete left;
  left = NULL;
}

void JitCompiler::ProcessStackCallback(long instr_id, StackInstr* instr,
					   long &instr_index, long params) {
  long non_params;
  if(params < 0) {
    non_params = 0;
  }
  else {
    non_params = working_stack.size() - params;
  }
  
#ifdef _DEBUG
  wcout << L"Return: params=" << params << L", non-params=" << non_params << endl;
#endif
  
  stack<RegInstr*> regs;
  stack<long> dirty_regs;
  long reg_offset = TMP_REG_0;  

  stack<RegInstr*> xmms;
  stack<long> dirty_xmms;
  long xmm_offset = TMP_XMM_0;
  
  long i = 0;     
  for(deque<RegInstr*>::reverse_iterator iter = working_stack.rbegin();
      iter != working_stack.rend(); ++iter) {
    RegInstr* left = (*iter);
    if(i < non_params) {
      switch(left->GetType()) {
      case REG_INT:
	move_reg_mem(left->GetRegister()->GetRegister(), reg_offset, RBP);
	dirty_regs.push(reg_offset);
	regs.push(left);
	reg_offset -= sizeof(long);
	break;

      case REG_FLOAT:
	move_xreg_mem(left->GetRegister()->GetRegister(), xmm_offset, RBP);
	dirty_xmms.push(xmm_offset);
	xmms.push(left);
	xmm_offset -= sizeof(double);
	break;

      default:
	break;
      }
      // update
      i++;
    }
  }

#ifdef _DEBUG
  assert(reg_offset >= TMP_REG_5);
  assert(xmm_offset >= TMP_XMM_2);
#endif

  if(dirty_regs.size() > 6 || dirty_xmms.size() > 3 ) {
    compile_success = false;
  }

  ProcessReturn(params);
  
  // save registers
  push_reg(R15);
  push_reg(R14);
  push_reg(R13);
  push_reg(R8);
  
  // function values
  move_mem_reg(OP_STACK, RBP, R9);
  move_mem_reg(INSTANCE_MEM, RBP, R8);
  move_mem_reg(MTHD_ID, RBP, RCX);
  move_mem_reg(CLS_ID, RBP, RDX);
  move_imm_reg((long)instr, RSI);
  move_imm_reg(instr_id, RDI);  
  push_imm(instr_index - 1);
  push_mem(STACK_POS, RBP);
  
  // call function
  RegisterHolder* call_holder = GetRegister();
  move_imm_reg((long)JitCompiler::StackCallback, call_holder->GetRegister());
  
  call_reg(call_holder->GetRegister());
  add_imm_reg(16, RSP);
  
  // restore registers
  pop_reg(R8);
  pop_reg(R13);
  pop_reg(R14);
  pop_reg(R15);

  ReleaseRegister(call_holder);
  
  // restore register values
  while(!dirty_regs.empty()) {
    RegInstr* left = regs.top();
    move_mem_reg(dirty_regs.top(), RBP, left->GetRegister()->GetRegister());
    // update
    regs.pop();
    dirty_regs.pop();
  }
  
  while(!dirty_xmms.empty()) {
    RegInstr* left = xmms.top();
    move_mem_xreg(dirty_xmms.top(), RBP, left->GetRegister()->GetRegister());
    // update
    xmms.pop();
    dirty_xmms.pop();
  }
}

void JitCompiler::ProcessReturn(long params) {
  if(!working_stack.empty()) {
    RegisterHolder* op_stack_holder = GetRegister();
    move_mem_reg(OP_STACK, RBP, op_stack_holder->GetRegister());
    
    RegisterHolder* stack_pos_holder = GetRegister();
    move_mem_reg(STACK_POS, RBP, stack_pos_holder->GetRegister());    
    move_mem_reg(0, stack_pos_holder->GetRegister(), stack_pos_holder->GetRegister());
    shl_imm_reg(3, stack_pos_holder->GetRegister());
    add_reg_reg(stack_pos_holder->GetRegister(), op_stack_holder->GetRegister());  

    long non_params;
    if(params < 0) {
      non_params = 0;
    }
    else {
      non_params = working_stack.size() - params;
    }
#ifdef _DEBUG
    wcout << L"Return: params=" << params << L", non-params=" << non_params << endl;
#endif
    
    long i = 0;     
    for(deque<RegInstr*>::reverse_iterator iter = working_stack.rbegin(); 
	iter != working_stack.rend(); ++iter) {
      // skip non-params... processed above
      RegInstr* left = (*iter);
      if(i < non_params) {
	i++;
      }
      else {
	move_mem_reg(STACK_POS, RBP, stack_pos_holder->GetRegister());            
	switch(left->GetType()) {
	case IMM_INT:
	  move_imm_mem(left->GetOperand(), 0, op_stack_holder->GetRegister());
	  inc_mem(0, stack_pos_holder->GetRegister());
	  add_imm_reg(sizeof(long), op_stack_holder->GetRegister());
	  break;
	
	case MEM_INT: {
	  RegisterHolder* temp_holder = GetRegister();
	  move_mem_reg(left->GetOperand(), RBP, temp_holder->GetRegister());
	  move_reg_mem(temp_holder->GetRegister(), 0, op_stack_holder->GetRegister());
	  inc_mem(0, stack_pos_holder->GetRegister());
	  add_imm_reg(sizeof(long), op_stack_holder->GetRegister()); 
	  ReleaseRegister(temp_holder);
	}
	  break;
	
	case REG_INT:
	  move_reg_mem(left->GetRegister()->GetRegister(), 0, op_stack_holder->GetRegister());
	  inc_mem(0, stack_pos_holder->GetRegister());
	  add_imm_reg(sizeof(long), op_stack_holder->GetRegister()); 
	  break;
	
	case IMM_FLOAT:
	  move_imm_memx(left, 0, op_stack_holder->GetRegister());
	  inc_mem(0, stack_pos_holder->GetRegister());
	  add_imm_reg(sizeof(double), op_stack_holder->GetRegister()); 
	  break;
	
	case MEM_FLOAT: {       
	  RegisterHolder* temp_holder = GetXmmRegister();
	  move_mem_xreg(left->GetOperand(), RBP, temp_holder->GetRegister());
	  move_xreg_mem(temp_holder->GetRegister(), 0, op_stack_holder->GetRegister());
	  inc_mem(0, stack_pos_holder->GetRegister());
	  add_imm_reg(sizeof(double), op_stack_holder->GetRegister());
	  ReleaseXmmRegister(temp_holder); 
	}
	  break;
	
	case REG_FLOAT:
	  move_xreg_mem(left->GetRegister()->GetRegister(), 0, op_stack_holder->GetRegister());
	  inc_mem(0, stack_pos_holder->GetRegister());
	  add_imm_reg(sizeof(double), op_stack_holder->GetRegister());
	  break;
	}    
      }
    }
    ReleaseRegister(op_stack_holder);
    ReleaseRegister(stack_pos_holder);
    
    // clean up working stack
    if(params < 0) {
      params = working_stack.size();
    }
    for(long i = 0; i < params; i++) {
      RegInstr* left = working_stack.front();
      working_stack.pop_front();

      // release register
      switch(left->GetType()) {
      case REG_INT:
	ReleaseRegister(left->GetRegister());
	break;

      case REG_FLOAT:
	ReleaseXmmRegister(left->GetRegister());
	break;

      default:
	break;
      }
      // clean up
      delete left;
      left = NULL;
    }
  }
}

RegInstr* JitCompiler::ProcessIntFold(long left_imm, long right_imm, InstructionType type) {
  switch(type) {
  case AND_INT:
    return new RegInstr(IMM_INT, left_imm && right_imm);
    
  case OR_INT:
    return new RegInstr(IMM_INT, left_imm || right_imm);
    
  case ADD_INT:
    return new RegInstr(IMM_INT, left_imm + right_imm);
    
  case SUB_INT:
    return new RegInstr(IMM_INT, left_imm - right_imm);
    
  case MUL_INT:
    return new RegInstr(IMM_INT, left_imm * right_imm);
    
  case DIV_INT:
    return new RegInstr(IMM_INT, left_imm / right_imm);
    
  case MOD_INT:
    return new RegInstr(IMM_INT, left_imm % right_imm);
    
  case SHL_INT:
    return new RegInstr(IMM_INT, left_imm << right_imm);
    
  case SHR_INT:
    return new RegInstr(IMM_INT, left_imm >> right_imm);
    
  case BIT_AND_INT:
    return new RegInstr(IMM_INT, left_imm & right_imm);
    
  case BIT_OR_INT:
    return new RegInstr(IMM_INT, left_imm | right_imm);
    
  case BIT_XOR_INT:
    return new RegInstr(IMM_INT, left_imm ^ right_imm);
    
  case LES_INT:	
    return new RegInstr(IMM_INT, left_imm < right_imm);
    
  case GTR_INT:
    return new RegInstr(IMM_INT, left_imm > right_imm);
    
  case EQL_INT:
    return new RegInstr(IMM_INT, left_imm == right_imm);
    
  case NEQL_INT:
    return new RegInstr(IMM_INT, left_imm != right_imm);
    
  case LES_EQL_INT:
    return new RegInstr(IMM_INT, left_imm <= right_imm);
    
  case GTR_EQL_INT:
    return new RegInstr(IMM_INT, left_imm >= right_imm);
    
  default:
    return NULL;
  }
}

void JitCompiler::ProcessIntCalculation(StackInstr* instruction) {
  RegInstr* left = working_stack.front();
  working_stack.pop_front();

  RegInstr* right = working_stack.front();
  working_stack.pop_front();
  
  switch(left->GetType()) {
    // intermidate
  case IMM_INT:
    switch(right->GetType()) {
    case IMM_INT:
      working_stack.push_front(ProcessIntFold(left->GetOperand(), 
					      right->GetOperand(), 
					      instruction->GetType()));
      break;
      
    case REG_INT: {
      RegisterHolder* imm_holder = GetRegister();
      move_imm_reg(left->GetOperand(), imm_holder->GetRegister());
      RegisterHolder* holder = right->GetRegister();
      
      math_reg_reg(holder->GetRegister(), imm_holder->GetRegister(), 
		   instruction->GetType());
      
      ReleaseRegister(holder);
      working_stack.push_front(new RegInstr(imm_holder));
    }
      break;
      
    case MEM_INT: {
      RegisterHolder* holder = GetRegister();
      move_mem_reg(right->GetOperand(), RBP, holder->GetRegister());

      RegisterHolder* imm_holder = GetRegister();
      move_imm_reg(left->GetOperand(), imm_holder->GetRegister());

      math_reg_reg(holder->GetRegister(), imm_holder->GetRegister(), 
		   instruction->GetType());
      
      ReleaseRegister(holder);
      working_stack.push_front(new RegInstr(imm_holder));
    }
      break;

    default:
      break;
    }	    
    break; 

    // register
  case REG_INT:
    switch(right->GetType()) {
    case IMM_INT: {
      RegisterHolder* holder = left->GetRegister();
      math_imm_reg(right->GetOperand(), holder->GetRegister(), 
		   instruction->GetType());
      working_stack.push_front(new RegInstr(holder));
    }
      break;
    case REG_INT: {
      RegisterHolder* holder = right->GetRegister();
      math_reg_reg(holder->GetRegister(), left->GetRegister()->GetRegister(), instruction->GetType());
      working_stack.push_front(new RegInstr(left->GetRegister()));
      ReleaseRegister(holder);
    }
      break;
    case MEM_INT: {
      RegisterHolder* rhs = GetRegister();
      move_mem_reg(right->GetOperand(), RBP, rhs->GetRegister());
      RegisterHolder* lhs = left->GetRegister();
      math_reg_reg(rhs->GetRegister(), lhs->GetRegister(), instruction->GetType());
      ReleaseRegister(rhs);
      working_stack.push_front(new RegInstr(lhs));
    }
      break;

    default:
      break;
    }
    break;

    // memory
  case MEM_INT:
    switch(right->GetType()) {
    case IMM_INT: {
      RegisterHolder* holder = GetRegister();
      move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
      math_imm_reg(right->GetOperand(), holder->GetRegister(),
		   instruction->GetType());
      working_stack.push_front(new RegInstr(holder));
    }
      break;
    case REG_INT: {
      RegisterHolder* rhs = GetRegister();
      move_mem_reg(left->GetOperand(), RBP, rhs->GetRegister());
      RegisterHolder* lhs = right->GetRegister();
      math_reg_reg(lhs->GetRegister(), rhs->GetRegister(), instruction->GetType());
      ReleaseRegister(lhs);
      working_stack.push_front(new RegInstr(rhs));
    }
      break;
    case MEM_INT: {
      RegisterHolder* holder = GetRegister();
      move_mem_reg(left->GetOperand(), RBP, holder->GetRegister());
      math_mem_reg(right->GetOperand(), holder->GetRegister(), instruction->GetType());
      working_stack.push_front(new RegInstr(holder));
    }
      break;

    default:
      break;
    }
    break;

  default:
    break;
  }
  
  delete left;
  left = NULL;
    
  delete right;
  right = NULL;
}

void JitCompiler::ProcessFloatCalculation(StackInstr* instruction) {
  RegInstr* left = working_stack.front();
  working_stack.pop_front();
  
  RegInstr* right = working_stack.front();
  working_stack.pop_front();

  InstructionType type = instruction->GetType();
  switch(left->GetType()) {
    // intermidate
  case IMM_FLOAT:
    switch(right->GetType()) {
    case IMM_FLOAT: {
      RegisterHolder* left_holder = GetXmmRegister();
      move_imm_xreg(left, left_holder->GetRegister());      
      RegisterHolder* right_holder = GetXmmRegister();
      move_imm_xreg(right, right_holder->GetRegister());      
      
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
	math_xreg_xreg(left_holder->GetRegister(), right_holder->GetRegister(), 
		       instruction->GetType());
	ReleaseXmmRegister(left_holder);
	working_stack.push_front(new RegInstr(right_holder));
      }
      else {
	math_xreg_xreg(right_holder->GetRegister(), left_holder->GetRegister(), 
		       instruction->GetType());
	ReleaseXmmRegister(right_holder);
	working_stack.push_front(new RegInstr(left_holder));
      }
    }
      break;
      
    case REG_FLOAT: {      
      RegisterHolder* imm_holder = GetXmmRegister();
      move_imm_xreg(left, imm_holder->GetRegister());
      
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
        math_xreg_xreg(imm_holder->GetRegister(), right->GetRegister()->GetRegister(), type);
        ReleaseXmmRegister(imm_holder);
        working_stack.push_front(new RegInstr(right->GetRegister()));
      }
      else {
        math_xreg_xreg(right->GetRegister()->GetRegister(), imm_holder->GetRegister(), type);
        ReleaseXmmRegister(right->GetRegister());
        working_stack.push_front(new RegInstr(imm_holder));
      }
    }
      break;

    case MEM_FLOAT:
    case MEM_INT: {
      RegisterHolder* holder = GetXmmRegister();
      move_mem_xreg(right->GetOperand(), RBP, holder->GetRegister());

      RegisterHolder* imm_holder = GetXmmRegister();
      move_imm_xreg(left, imm_holder->GetRegister());

      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
        math_xreg_xreg(imm_holder->GetRegister(), holder->GetRegister(), type);
        ReleaseXmmRegister(imm_holder);
        working_stack.push_front(new RegInstr(holder));
      }
      else {
        math_xreg_xreg(holder->GetRegister(), imm_holder->GetRegister(), type);
        ReleaseXmmRegister(holder);
        working_stack.push_front(new RegInstr(imm_holder));
      }
    }
      break;

    default:
      break;
    }	    
    break; 

    // register
  case REG_FLOAT:
    switch(right->GetType()) {
    case IMM_FLOAT: {
      RegisterHolder* right_holder = GetXmmRegister();
      move_imm_xreg(right, right_holder->GetRegister());

      RegisterHolder* left_holder = left->GetRegister();      
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
	math_xreg_xreg(left_holder->GetRegister(), right_holder->GetRegister(), instruction->GetType());
	ReleaseXmmRegister(left_holder);      
	working_stack.push_front(new RegInstr(right_holder));
      }
      else {
	math_xreg_xreg(right_holder->GetRegister(), left_holder->GetRegister(), instruction->GetType());
	ReleaseXmmRegister(right_holder);      
	working_stack.push_front(new RegInstr(left_holder));
      }
    }
      break;

    case REG_FLOAT: {
      RegisterHolder* holder = right->GetRegister();
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
	math_xreg_xreg(left->GetRegister()->GetRegister(), holder->GetRegister(), instruction->GetType());
	working_stack.push_front(new RegInstr(holder));
	ReleaseXmmRegister(left->GetRegister());
      }
      else {
	math_xreg_xreg(holder->GetRegister(), left->GetRegister()->GetRegister(), instruction->GetType());
	working_stack.push_front(new RegInstr(left->GetRegister()));
	ReleaseXmmRegister(holder);
      }
    }
      break;
      
    case MEM_FLOAT:
    case MEM_INT: {
      RegisterHolder* holder = left->GetRegister();
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
	RegisterHolder* right_holder = GetXmmRegister();
	move_mem_xreg(right->GetOperand(), RBP, right_holder->GetRegister());
	math_xreg_xreg(holder->GetRegister(), right_holder->GetRegister(), instruction->GetType());
	ReleaseXmmRegister(holder);
	working_stack.push_front(new RegInstr(right_holder));
      }
      else {
	math_mem_xreg(right->GetOperand(), holder->GetRegister(), instruction->GetType());
	working_stack.push_front(new RegInstr(holder));
      }
    }
      break;

    default:
      break;
    }
    break;

    // memory
  case MEM_FLOAT:
  case MEM_INT:
    switch(right->GetType()) {
    case IMM_FLOAT: {
      RegisterHolder* holder = GetXmmRegister();
      move_mem_xreg(left->GetOperand(), RBP, holder->GetRegister());
      
      RegisterHolder* imm_holder = GetXmmRegister();
      move_imm_xreg(right, imm_holder->GetRegister());
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
        math_xreg_xreg(holder->GetRegister(), imm_holder->GetRegister(), type);
        ReleaseXmmRegister(holder);
        working_stack.push_front(new RegInstr(imm_holder));
      }
      else {
        math_xreg_xreg(imm_holder->GetRegister(), holder->GetRegister(), type);
        ReleaseXmmRegister(imm_holder);
        working_stack.push_front(new RegInstr(holder));
      }
    }
      break;
      
    case REG_FLOAT: {
      RegisterHolder* holder = right->GetRegister();
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
	math_mem_xreg(left->GetOperand(), holder->GetRegister(), instruction->GetType());
	working_stack.push_front(new RegInstr(holder));
      }
      else {
	RegisterHolder* right_holder = GetXmmRegister();
	move_mem_xreg(left->GetOperand(), RBP, right_holder->GetRegister());
	math_xreg_xreg(holder->GetRegister(), right_holder->GetRegister(), instruction->GetType());
	ReleaseXmmRegister(holder);
	working_stack.push_front(new RegInstr(right_holder));
      }
    }
      break;
      
    case MEM_FLOAT:
    case MEM_INT: {
      RegisterHolder* left_holder = GetXmmRegister();
      move_mem_xreg(left->GetOperand(), RBP, left_holder->GetRegister());

      RegisterHolder* right_holder = GetXmmRegister();
      move_mem_xreg(right->GetOperand(), RBP, right_holder->GetRegister());
      if(type == LES_FLOAT || type == LES_EQL_FLOAT) {
	math_xreg_xreg(left_holder->GetRegister(), right_holder->GetRegister(),  
		       instruction->GetType());
	ReleaseXmmRegister(left_holder);
	working_stack.push_front(new RegInstr(right_holder));
      }
      else {
	math_xreg_xreg(right_holder->GetRegister(), left_holder->GetRegister(),  
		       instruction->GetType());	
	ReleaseXmmRegister(right_holder);
	working_stack.push_front(new RegInstr(left_holder));
      }
    }
      break;

    default:
      break;
    }
    break;

  default:
    break;
  }

  delete left;
  left = NULL;
    
  delete right;
  right = NULL;
}

/////////////////// OPERATIONS ///////////////////

void JitCompiler::move_reg_reg(Register src, Register dest) {
  if(src != dest) {
#ifdef _DEBUG
    wcout << L"  " << (++instr_count) << L": [movq %" << GetRegisterName(src) 
	  << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
    // encode
    AddMachineCode(ROB(src, dest));
    AddMachineCode(0x89);
    unsigned char code = 0xc0;
    // write value
    RegisterEncode3(code, 2, src);
    RegisterEncode3(code, 5, dest);
    AddMachineCode(code);
  }
}

void JitCompiler::move_reg_mem8(Register src, long offset, Register dest) { 
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movb %" << GetRegisterName(src) 
	<< L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
	<< endl;
#endif
  // encode
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x88);
  AddMachineCode(ModRM(dest, src));
  // write value
  AddImm(offset);
}

void JitCompiler::move_reg_mem32(Register src, long offset, Register dest) { 
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movw %" << GetRegisterName(src) 
       << L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
       << endl;
#endif
  // encode
  AddMachineCode(RXB32(src, dest));
  AddMachineCode(0x89);
  AddMachineCode(ModRM(dest, src));
  // write value
  AddImm(offset);
}
    
void JitCompiler::move_reg_mem(Register src, long offset, Register dest) { 
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movl %" << GetRegisterName(src) 
	<< L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
	<< endl;
#endif
  // encode
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x89);
  AddMachineCode(ModRM(dest, src));
  // write value
  AddImm(offset);
}

void JitCompiler::move_mem8_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movb " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest)
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0xb6);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::move_mem32_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movw " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest)
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB32(dest, src));
  AddMachineCode(0x8b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::move_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest)
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x8b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}
    
void JitCompiler::move_imm_memx(RegInstr* instr, long offset, Register dest) {
  RegisterHolder* tmp_holder = GetXmmRegister();
  move_imm_xreg(instr, tmp_holder->GetRegister());
  move_xreg_mem(tmp_holder->GetRegister(), offset, dest);
  ReleaseXmmRegister(tmp_holder);
}

void JitCompiler::move_imm_mem8(long imm, long offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movb $" << imm << L", " << offset 
	<< L"(%" << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
  // encode
  AddMachineCode(XB(dest));
  AddMachineCode(0xc6);
  unsigned char code = 0x80;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  // write value
  AddImm(offset);
  AddMachineCode(imm);
}

void JitCompiler::move_imm_mem(long imm, long offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movq $" << imm << L", " << offset 
	<< L"(%" << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
  // encode
  AddMachineCode(XB(dest));
  AddMachineCode(0xc7); 
  unsigned char code = 0x80;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  // write value
  AddImm(offset);
  AddImm(imm);
}

void JitCompiler::move_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movq $" << imm << L", %" 
	<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(XB(reg));
  unsigned char code = 0xb8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm64(imm);
}

void JitCompiler::move_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());  
  move_mem_xreg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}
    
void JitCompiler::move_mem_xreg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movsd " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest)
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x10);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}
    
void JitCompiler::move_xreg_mem(Register src, long offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movsd %" << GetRegisterName(src) 
	<< L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
	<< endl;
#endif 
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x11);
  AddMachineCode(ModRM(dest, src));
  // write value
  AddImm(offset);
}
    
void JitCompiler::move_xreg_xreg(Register src, Register dest) {
  if(src != dest) {
#ifdef _DEBUG
    wcout << L"  " << (++instr_count) << L": [movsd %" << GetRegisterName(src) 
	  << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
    // encode
    AddMachineCode(0xf2);
    AddMachineCode(ROB(src, dest));
    AddMachineCode(0x0f);
    AddMachineCode(0x11);
    unsigned char code = 0xc0;
    // write value
    RegisterEncode3(code, 2, src);
    RegisterEncode3(code, 5, dest);
    AddMachineCode(code);
  }
}

bool JitCompiler::cond_jmp(InstructionType type) {
  if(instr_index >= method->GetInstructionCount()) {
    return false;
  }
  
  StackInstr* next_instr = method->GetInstruction(instr_index);
  if(next_instr->GetType() == JMP && next_instr->GetOperand2() > -1) {
    // if(false) {
#ifdef _DEBUG
    wcout << L"JMP: id=" << next_instr->GetOperand() << L", regs=" << aval_regs.size() << L"," << aux_regs.size() << endl;
#endif
    AddMachineCode(0x0f);

    //
    // jump if true
    //
    if(next_instr->GetOperand2() == 1) {
      switch(type) {
      case LES_INT:	
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jl]" << endl;
#endif
        AddMachineCode(0x8c);
        break;

      case GTR_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jg]" << endl;
#endif
        AddMachineCode(0x8f);
        break;

      case EQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [je]" << endl;
#endif
        AddMachineCode(0x84);
        break;

      case NEQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jne]" << endl;
#endif
        AddMachineCode(0x85);
        break;

      case LES_EQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jle]" << endl;
#endif
        AddMachineCode(0x8e);
        break;
        
      case GTR_EQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jge]" << endl;
#endif
        AddMachineCode(0x8d);
        break;

      default:
	break;
      }  
    }
    //
    // jump - false
    //
    else {
      switch(type) {
      case LES_INT:	
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jge]" << endl;
#endif
        AddMachineCode(0x8d);
        break;

      case GTR_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jle]" << endl;
#endif
        AddMachineCode(0x8e);
        break;

      case EQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jne]" << endl;
#endif
        AddMachineCode(0x85);
        break;

      case NEQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [je]" << endl;
#endif
        AddMachineCode(0x84);
        break;

      case LES_EQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jg]" << endl;
#endif
        AddMachineCode(0x8f);
        break;
        
      case GTR_EQL_INT:
#ifdef _DEBUG
        wcout << L"  " << (++instr_count) << L": [jl]" << endl;
#endif
        AddMachineCode(0x8c);
        break;

      default:
	break;
      }  
    }    
    // store update index
    jump_table.insert(pair<long, StackInstr*>(code_index, next_instr));
    // temp offset
    AddImm(0);
    skip_jump = true;
    
    return true;
  }
  
  return false;
}

void JitCompiler::math_imm_reg(long imm, Register reg, InstructionType type) {
  switch(type) {
  case AND_INT:
    and_imm_reg(imm, reg);
    break;

  case OR_INT:
    or_imm_reg(imm, reg);
    break;
    
  case ADD_INT:
    add_imm_reg(imm, reg);
    break;

  case SUB_INT:
    sub_imm_reg(imm, reg);
    break;

  case MUL_INT:
    mul_imm_reg(imm, reg);
    break;

  case DIV_INT:
    div_imm_reg(imm, reg);
    break;
    
  case MOD_INT:
    div_imm_reg(imm, reg, true);
    break;

  case SHL_INT:
    shl_imm_reg(imm, reg);
    break;
    
  case SHR_INT:
    shr_imm_reg(imm, reg);
    break;

  case BIT_AND_INT:
    and_imm_reg(imm, reg);
    break;
    
  case BIT_OR_INT:
    or_imm_reg(imm, reg);
    break;
    
  case BIT_XOR_INT:
    xor_imm_reg(imm, reg);
    break;
    
  case LES_INT:	
  case GTR_INT:
  case EQL_INT:
  case NEQL_INT:
  case LES_EQL_INT:
  case GTR_EQL_INT:
    cmp_imm_reg(imm, reg);
    if(!cond_jmp(type)) {
      cmov_reg(reg, type);
    }
    break;

  default:
    break;
  }
}

void JitCompiler::math_reg_reg(Register src, Register dest, InstructionType type) {
  switch(type) {
  case SHL_INT:
    shl_reg_reg(src, dest);
    break;
    
  case SHR_INT:
    shr_reg_reg(src, dest);
    break;
  case AND_INT:
    and_reg_reg(src, dest);
    break;

  case OR_INT:
    or_reg_reg(src, dest);
    break;
    
  case ADD_INT:
    add_reg_reg(src, dest);
    break;

  case SUB_INT:
    sub_reg_reg(src, dest);
    break;

  case MUL_INT:
    mul_reg_reg(dest, src);
    break;

  case DIV_INT:
    div_reg_reg(src, dest);
    break;

  case MOD_INT:
    div_reg_reg(src, dest, true);
    break;

  case BIT_AND_INT:
    and_reg_reg(src, dest);
    break;

  case BIT_OR_INT:
    or_reg_reg(src, dest);
    break;

  case BIT_XOR_INT:
    xor_reg_reg(src, dest);
    break;
    
  case LES_INT:	
  case GTR_INT:
  case EQL_INT:
  case NEQL_INT:
  case LES_EQL_INT:
  case GTR_EQL_INT:
    cmp_reg_reg(src, dest);
    if(!cond_jmp(type)) {
      cmov_reg(dest, type);
    }
    break;

  default:
    break;
  }
}

void JitCompiler::math_mem_reg(long offset, Register reg, InstructionType type) {
  switch(type) {
  case SHL_INT:
    shl_mem_reg(offset, RBP, reg);
    break;

  case SHR_INT:
    shr_mem_reg(offset, RBP, reg);
    break;
    
  case AND_INT:
    and_mem_reg(offset, RBP, reg);
    break;

  case OR_INT:
    or_mem_reg(offset, RBP, reg);
    break;
    
  case ADD_INT:
    add_mem_reg(offset, RBP, reg);
    break;

  case SUB_INT:
    sub_mem_reg(offset, RBP, reg);
    break;

  case MUL_INT:
    mul_mem_reg(offset, RBP, reg);
    break;

  case DIV_INT:
    div_mem_reg(offset, RBP, reg, false);
    break;
    
  case MOD_INT:
    div_mem_reg(offset, RBP, reg, true);
    break;

  case BIT_AND_INT:
    and_mem_reg(offset, RBP, reg);
    break;

  case BIT_OR_INT:
    or_mem_reg(offset, RBP, reg);
    break;

  case BIT_XOR_INT:
    xor_mem_reg(offset, RBP, reg);
    break;
    
  case LES_INT:
  case LES_EQL_INT:
  case GTR_INT:
  case EQL_INT:
  case NEQL_INT:  
  case GTR_EQL_INT:
    cmp_mem_reg(offset, RBP, reg);
    if(!cond_jmp(type)) {
      cmov_reg(reg, type);
    }
    break;

  default:
    break;
  }
}

void JitCompiler::math_imm_xreg(RegInstr* instr, Register reg, InstructionType type) {
  switch(type) {
  case ADD_FLOAT:
    add_imm_xreg(instr, reg);
    break;

  case SUB_FLOAT:
    sub_imm_xreg(instr, reg);
    break;

  case MUL_FLOAT:
    mul_imm_xreg(instr, reg);
    break;

  case DIV_FLOAT:
    div_imm_xreg(instr, reg);
    break;
    
  case LES_FLOAT:
  case LES_EQL_FLOAT:
  case GTR_FLOAT:
  case EQL_FLOAT:
  case NEQL_FLOAT:
  case GTR_EQL_FLOAT:
    cmp_imm_xreg(instr, reg);
    break;

  default:
    break;
  }
}

void JitCompiler::math_mem_xreg(long offset, Register dest, InstructionType type) {
  RegisterHolder* holder = GetXmmRegister();
  move_mem_xreg(offset, RBP, holder->GetRegister());
  math_xreg_xreg(holder->GetRegister(), dest, type);
  ReleaseXmmRegister(holder);
}

void JitCompiler::math_xreg_xreg(Register src, Register dest, InstructionType type) {
  switch(type) {
  case ADD_FLOAT:
    add_xreg_xreg(src, dest);
    break;

  case SUB_FLOAT:
    sub_xreg_xreg(src, dest);
    break;

  case MUL_FLOAT:
    mul_xreg_xreg(src, dest);
    break;

  case DIV_FLOAT:
    div_xreg_xreg(src, dest);
    break;
    
  case LES_FLOAT:
  case LES_EQL_FLOAT:
  case GTR_FLOAT:
  case EQL_FLOAT:
  case NEQL_FLOAT:
  case GTR_EQL_FLOAT:
    cmp_xreg_xreg(src, dest);
    break;

  default:
    break;
  }
}    

void JitCompiler::cmp_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cmpq %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x39);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::cmp_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cmpq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x3b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}
    
void JitCompiler::cmp_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cmpq $" << imm << L", %"
	<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(XB(reg));
  AddMachineCode(0x81);
  unsigned char code = 0xf8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::cmov_reg(Register reg, InstructionType oper) {
  // set register to 0; if eflag than set to 1
  move_imm_reg(0, reg);
  RegisterHolder* true_holder = GetRegister();
  move_imm_reg(1, true_holder->GetRegister());
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cmovq %" 
	<< GetRegisterName(reg) << L", %" 
	<< GetRegisterName(true_holder->GetRegister()) << L" ]" << endl;
#endif
  // encode
  AddMachineCode(ROB(reg, true_holder->GetRegister()));
  AddMachineCode(0x0f);
  switch(oper) {    
  case GTR_INT:
    AddMachineCode(0x4f);
    break;

  case LES_INT:
    AddMachineCode(0x4c);
    break;
    
  case EQL_INT:
  case EQL_FLOAT:
    AddMachineCode(0x44);
    break;

  case NEQL_INT:
  case NEQL_FLOAT:
    AddMachineCode(0x45);
    break;
    
  case LES_FLOAT:
    AddMachineCode(0x47);
    break;
    
  case GTR_FLOAT:
    AddMachineCode(0x47);
    break;

  case LES_EQL_INT:
    AddMachineCode(0x4e);
    break;

  case GTR_EQL_INT:
    AddMachineCode(0x4d);
    break;
    
  case LES_EQL_FLOAT:
    AddMachineCode(0x43);
    break;

  case GTR_EQL_FLOAT:
    AddMachineCode(0x43);
    break;

  default:
    cerr << L">>> Unknown compare! <<<" << endl;
    exit(1);
    break;
  }
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, reg);
  RegisterEncode3(code, 5, true_holder->GetRegister());
  AddMachineCode(code);
  ReleaseRegister(true_holder);
}

void JitCompiler::add_imm_mem(long imm, long offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addq $" << imm << L", " 
	<< offset << L"(%"<< GetRegisterName(dest) << L")]" << endl;
#endif
  // encode
  AddMachineCode(XB(dest));
  AddMachineCode(0x81);
  AddMachineCode(ModRM(dest, RAX));
  // write value
  AddImm(offset);
  AddImm(imm);
}
    
void JitCompiler::add_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addq $" << imm << L", %"
	<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(B(reg));
  AddMachineCode(0x81);
  unsigned char code = 0xc0;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}
    
void JitCompiler::add_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  add_mem_xreg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}

void JitCompiler::sub_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  sub_mem_xreg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}

void JitCompiler::div_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  div_mem_xreg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}

void JitCompiler::mul_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  mul_mem_xreg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}

void JitCompiler::add_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addq %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x01);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::sub_xreg_xreg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subsd %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x5c);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);	     
}

void JitCompiler::mul_xreg_xreg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [mulsd %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(ROB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x59);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}

void JitCompiler::div_xreg_xreg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [divsd %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x5e);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}

void JitCompiler::add_xreg_xreg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addsd %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x58);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}
    
void JitCompiler::add_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x03);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::add_mem_xreg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addsd " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x58);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);  
}

void JitCompiler::sub_mem_xreg(long offset, Register src, Register dest) {
  RegisterHolder* holder = GetXmmRegister();
  move_mem_xreg(offset, src, holder->GetRegister());
  sub_xreg_xreg(dest, holder->GetRegister());
  move_xreg_xreg(holder->GetRegister(), dest);
  ReleaseXmmRegister(holder);
}

void JitCompiler::mul_mem_xreg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [mulsd " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x59);
  AddMachineCode(ModRM(src, dest));
  AddImm(offset);
}

void JitCompiler::div_mem_xreg(long offset, Register src, Register dest) {
  RegisterHolder* holder = GetXmmRegister();
  move_mem_xreg(offset, src, holder->GetRegister());
  div_xreg_xreg(dest, holder->GetRegister());
  move_xreg_xreg(holder->GetRegister(), dest);
  ReleaseXmmRegister(holder);
}

void JitCompiler::sub_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subq $" << imm << L", %"
	<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(B(reg));
  AddMachineCode(0x81);
  unsigned char code = 0xe8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  AddImm(imm);
}

void JitCompiler::sub_imm_mem(long imm, long offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subq $" << imm << L", " 
	<< offset << L"(%"<< GetRegisterName(dest) << L")]" << endl;
#endif
  // encode
  AddMachineCode(XB(dest));
  AddMachineCode(0x81);
  AddMachineCode(ModRM(dest, RBP));
  // write value
  AddImm(offset);
  AddImm(imm);
}

void JitCompiler::sub_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subq %" << GetRegisterName(src)
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x29);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::sub_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif

  // encode
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x2b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::mul_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [imuq $" << imm 
	<< L", %"<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(reg, reg));
  AddMachineCode(0x69);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, reg);
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::mul_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [imuq %" 
	<< GetRegisterName(src) << L", %"<< GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0xaf);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::mul_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [imuq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0xaf);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::div_imm_reg(long imm, Register reg, bool is_mod) {
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(imm, imm_holder->GetRegister());
  div_reg_reg(imm_holder->GetRegister(), reg, is_mod);
  ReleaseRegister(imm_holder);
}

void JitCompiler::div_mem_reg(long offset, Register src,
				  Register dest, bool is_mod) {
  if(is_mod) {
    if(dest != RDX) {
      move_reg_mem(RDX, TMP_REG_1, RBP);
    }
    move_reg_mem(RAX, TMP_REG_0, RBP);
  }
  else {
    if(dest != RAX) {
      move_reg_mem(RAX, TMP_REG_0, RBP);
    }
    move_reg_mem(RDX, TMP_REG_1, RBP);
  }

  // ============
  move_reg_reg(dest, RAX);
  AddMachineCode(0x48); // cdq
  AddMachineCode(0x99);
  
  // encode
  AddMachineCode(XB(src));
  AddMachineCode(0xf7);
  AddMachineCode(ModRM(src, RDI));
  // write value
  AddImm(offset);
  
#ifdef _DEBUG
  if(is_mod) {
    wcout << L"  " << (++instr_count) << L": [imod " << offset << L"(%" 
	  << GetRegisterName(src) << L")]" << endl;
  }
  else {
    wcout << L"  " << (++instr_count) << L": [idiv " << offset << L"(%" 
	  << GetRegisterName(src) << L")]" << endl;
  }
#endif
  // ============

  if(is_mod) {
    if(dest != RDX) {
      move_reg_reg(RDX, dest);
      move_mem_reg(TMP_REG_1, RBP, RDX);
    }

    if(dest != RAX) {
      move_mem_reg(TMP_REG_0, RBP, RAX);
    }
  }
  else {
    if(dest != RAX) {
      move_reg_reg(RAX, dest);
      move_mem_reg(TMP_REG_0, RBP, RAX);
    }
    
    if(dest != RDX) {
      move_mem_reg(TMP_REG_1, RBP, RDX);
    }
  }
}

void JitCompiler::div_reg_reg(Register src, Register dest, bool is_mod) {
  if(is_mod) {
    if(dest != RDX) {
      move_reg_mem(RDX, TMP_REG_1, RBP);
    }
    move_reg_mem(RAX, TMP_REG_0, RBP);
  }
  else {
    if(dest != RAX) {
      move_reg_mem(RAX, TMP_REG_0, RBP);
    }
    move_reg_mem(RDX, TMP_REG_1, RBP);
  }
  
  // ============
  move_reg_reg(dest, RAX);
  AddMachineCode(0x48); // cdq
  AddMachineCode(0x99);
  
  if(src != RAX && src != RDX) {
    // encode
    AddMachineCode(B(src));
    AddMachineCode(0xf7);
    unsigned char code = 0xf8;
    // write value
    RegisterEncode3(code, 5, src);
    AddMachineCode(code);
    
#ifdef _DEBUG
    if(is_mod) {
      wcout << L"  " << (++instr_count) << L": [imod %" 
	    << GetRegisterName(src) << L"]" << endl;
    }
    else {
      wcout << L"  " << (++instr_count) << L": [idiv %" 
	    << GetRegisterName(src) << L"]" << endl;
    }
#endif
  }
  else {
    // encode
    AddMachineCode(XB(RBP));
    AddMachineCode(0xf7);
    AddMachineCode(ModRM(RBP, RDI));
    // write value
    if(src == RAX) {
      AddImm(TMP_REG_0);
    }
    else {
      AddImm(TMP_REG_1);
    }
    
#ifdef _DEBUG
    if(is_mod) {
      wcout << L"  " << (++instr_count) << L": [imod " << TMP_REG_0 << L"(%" 
	    << GetRegisterName(RBP) << L")]" << endl;
    }
    else {
      wcout << L"  " << (++instr_count) << L": [idiv " << TMP_REG_0 << L"(%" 
	    << GetRegisterName(RBP) << L")]" << endl;
    }
#endif
  }
  // ============
  
  if(is_mod) {
    if(dest != RDX) {
      move_reg_reg(RDX, dest);
      move_mem_reg(TMP_REG_1, RBP, RDX);
    }

    if(dest != RAX) {
      move_mem_reg(TMP_REG_0, RBP, RAX);
    }
  }
  else {
    if(dest != RAX) {
      move_reg_reg(RAX, dest);
      move_mem_reg(TMP_REG_0, RBP, RAX);
    }
     
    if(dest != RDX) {
      move_mem_reg(TMP_REG_1, RBP, RDX);
    }
  }
}

void JitCompiler::dec_reg(Register dest) {
  AddMachineCode(B(dest));
  unsigned char code = 0x48;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [decq %" 
	<< GetRegisterName(dest) << L"]" << endl;
#endif
}

void JitCompiler::dec_mem(long offset, Register dest) {
  AddMachineCode(XB(dest));
  AddMachineCode(0xff);
  unsigned char code = 0x88;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddImm(offset);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [decq " << offset << L"(%" 
	<< GetRegisterName(dest) << L")" << L"]" << endl;
#endif
}

void JitCompiler::inc_mem(long offset, Register dest) {
  AddMachineCode(XB(dest));
  AddMachineCode(0xff);
  unsigned char code = 0x80;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddImm(offset);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [incq " << offset << L"(%" 
	<< GetRegisterName(dest) << L")" << L"]" << endl;
#endif
}

void JitCompiler::shl_imm_reg(long value, Register dest) {
  AddMachineCode(B(dest));
  AddMachineCode(0xc1);
  unsigned char code = 0xe0;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddMachineCode(value);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shlq $" << value << L", %" 
	<< GetRegisterName(dest) << L"]" << endl;
#endif
}

void JitCompiler::shl_reg_reg(Register src, Register dest)
{
  Register old_dest;
  RegisterHolder* reg_holder = NULL;
  if(dest == RCX) {
    reg_holder = GetRegister();
    old_dest = dest;
    dest = reg_holder->GetRegister();
    move_reg_reg(old_dest, dest);
  }
  
  if(src != RCX) {
    move_reg_mem(RCX, TMP_REG_0, RBP);
    move_reg_reg(src, RCX);
  }
  
  // --------------------

  // encode
  AddMachineCode(B(dest));
  AddMachineCode(0xd3);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, RSP);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shlq %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif

  // --------------------
  
  if(src != RCX) {
    move_mem_reg(TMP_REG_0, RBP, RCX);
  }
  
  if(reg_holder) {
    move_reg_reg(dest, old_dest);
    ReleaseRegister(reg_holder);
  }
}

void JitCompiler::shl_mem_reg(long offset, Register src, Register dest) 
{
  RegisterHolder* mem_holder = GetRegister();
  move_mem_reg(offset, src, mem_holder->GetRegister());
  shl_reg_reg(mem_holder->GetRegister(), dest);
  ReleaseRegister(mem_holder);
}

void JitCompiler::shr_imm_reg(long value, Register dest) {
  AddMachineCode(B(dest));
  AddMachineCode(0xc1);
  unsigned char code = 0xe8;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddMachineCode(value);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shrq $" << value << L", %" 
	<< GetRegisterName(dest) << L"]" << endl;
#endif
}

void JitCompiler::shr_reg_reg(Register src, Register dest)
{
  Register old_dest;
  RegisterHolder* reg_holder = NULL;
  if(dest == RCX) {
    reg_holder = GetRegister();
    old_dest = dest;
    dest = reg_holder->GetRegister();
    move_reg_reg(old_dest, dest);
  }
  
  if(src != RCX) {
    move_reg_mem(RCX, TMP_REG_0, RBP);
    move_reg_reg(src, RCX);
  }
  
  // --------------------
  
  // encode
  AddMachineCode(B(dest));
  AddMachineCode(0xd3);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, RBP);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shrq %" << GetRegisterName(RCX) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif

  // --------------------
  
  if(src != RCX) {
    move_mem_reg(TMP_REG_0, RBP, RCX);
  }
  
  if(reg_holder) {
    move_reg_reg(dest, old_dest);
    ReleaseRegister(reg_holder);
  }
}

void JitCompiler::shr_mem_reg(long offset, Register src, Register dest) 
{
  RegisterHolder* mem_holder = GetRegister();
  move_mem_reg(offset, src, mem_holder->GetRegister());
  shr_reg_reg(mem_holder->GetRegister(), dest);
  ReleaseRegister(mem_holder);
}

void JitCompiler::push_mem(long offset, Register dest) {
  AddMachineCode(B(dest));
  AddMachineCode(0xff);
  unsigned char code = 0xb0;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddImm(offset);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [pushq " << offset << L"(%" 
	<< GetRegisterName(dest) << L")" << L"]" << endl;
#endif
}

void JitCompiler::push_reg(Register reg) {
  AddMachineCode(B(reg));
  unsigned char code = 0x50;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [pushq %" << GetRegisterName(reg) 
	<< L"]" << endl;
#endif
}

void JitCompiler::push_imm(long value) {
  AddMachineCode(0x68);
  AddImm(value);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [pushq $" << value << L"]" << endl;
#endif
}

void JitCompiler::pop_reg(Register reg) {
  AddMachineCode(B(reg));  
  unsigned char code = 0x58;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [popq %" << GetRegisterName(reg) 
	<< L"]" << endl;
#endif
}

void JitCompiler::call_reg(Register reg) {
  AddMachineCode(B(reg));  
  AddMachineCode(0xff);
  unsigned char code = 0xd0;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [call %" << GetRegisterName(reg)
	<< L"]" << endl;
#endif
}

void JitCompiler::cmp_xreg_xreg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [ucomisd %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x66);
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x2e);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}

void JitCompiler::cmp_mem_xreg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [ucomisd " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(0x66);
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x2e);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::cmp_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  cmp_mem_xreg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}

void JitCompiler::cvt_xreg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cvtsd2si %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(ROB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x2c);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}

void JitCompiler::round_imm_xreg(RegInstr* instr, Register reg, bool is_floor) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  round_mem_xreg(0, imm_holder->GetRegister(), reg, is_floor);
  ReleaseRegister(imm_holder);
}

void JitCompiler::round_mem_xreg(long offset, Register src, Register dest, bool is_floor) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << (is_floor ? ": [floor " : ": [ceil ") 
	<< offset << L"(%" << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  
  AddMachineCode(0x66);
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x3a);
  AddMachineCode(0x0b);
  // memory
  AddMachineCode(ModRM(src, dest));
  AddImm(offset);
  // mode
  if(is_floor) {
    AddMachineCode(0x03);
  }
  else {
    AddMachineCode(0x05);
  }
}

void JitCompiler::round_xreg_xreg(Register src, Register dest, bool is_floor) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << (is_floor ? ": [floor %" : ": [ceil %") 
	<< GetRegisterName(src) << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  
  AddMachineCode(0x66);
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x0f);
  AddMachineCode(0x3a);
  AddMachineCode(0x0b);
  // registers
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  // mode
  if(is_floor) {
    AddMachineCode(0x03);
  }
  else {
    AddMachineCode(0x05);
  }
}

void JitCompiler::cvt_imm_reg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  cvt_mem_reg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}

void JitCompiler::cvt_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cvtsd2si " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x2c);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::cvt_reg_xreg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cvtsi2sd %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(ROB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x2a);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}

void JitCompiler::cvt_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());
  cvt_reg_xreg(imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}

void JitCompiler::cvt_mem_xreg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cvtsi2sd " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(RXB(dest, src));
  AddMachineCode(0x0f);
  AddMachineCode(0x2a);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::and_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [andq $" << imm << L", %"
	<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(B(reg));
  AddMachineCode(0x81);
  unsigned char code = 0xe0;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::and_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [andq %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x21);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::and_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [andq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x23);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::or_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [orq $" << imm << L", %"
	<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(B(reg));
  AddMachineCode(0x81);
  unsigned char code = 0xc8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::or_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [orq %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x09);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::or_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [orq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x0b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::xor_imm_reg(long imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [xorq $" << imm << L", %"
	<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(B(reg));
  AddMachineCode(0x81);
  unsigned char code = 0xf0;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::xor_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [xorq %" << GetRegisterName(src) 
	<< L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(ROB(src, dest));
  AddMachineCode(0x31);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::xor_mem_reg(long offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [xorq " << offset << L"(%" 
	<< GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
	<< L"]" << endl;
#endif
  // encode
  AddMachineCode(RXB(src, dest));
  AddMachineCode(0x33);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

/********************************
 * JitExecutorIA32 class
 ********************************/
StackProgram* JitExecutorIA32::program;
void JitExecutorIA32::Initialize(StackProgram* p) {
  program = p;
}

long JitExecutorIA32::ExecuteMachineCode(long cls_id, long mthd_id, long* inst, unsigned char* code, 
					 const long code_size, long* op_stack, long *stack_pos,
					 StackFrame** call_stack, long* call_stack_pos) {
  // create function
  jit_fun_ptr jit_fun = (jit_fun_ptr)code;
  
  // execute
#ifdef _TIMING
  clock_t start = clock();
  long result = jit_fun(cls_id, mthd_id, method->GetClass()->GetClassMemory(), 
			inst, op_stack, stack_pos);
  wcout << L"JIT execution: method='" << method->GetName() << L"', time=" 
	<< (double)(clock() - start) / CLOCKS_PER_SEC << L" second(s)." << endl;
  return result;
#else
  // execute
  return jit_fun(cls_id, mthd_id, method->GetClass()->GetClassMemory(), 
		 inst, op_stack, stack_pos, call_stack, call_stack_pos);
#endif
}