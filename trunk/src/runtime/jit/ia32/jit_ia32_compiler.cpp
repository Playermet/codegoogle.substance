#include "jit_ia32_compiler.h"

using namespace jit;

RegInstr* JitCompiler::ProcessIntFold(long left_imm, long right_imm, jit::InstructionType type) {
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
      move_mem_reg(right->GetOperand(), EBP, holder->GetRegister());

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
      RegisterHolder* lhs = left->GetRegister();
      RegisterHolder* rhs = GetRegister();
      move_mem_reg(right->GetOperand(), EBP, rhs->GetRegister());
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
      move_mem_reg(left->GetOperand(), EBP, holder->GetRegister());
      math_imm_reg(right->GetOperand(), holder->GetRegister(),
		   instruction->GetType());
      working_stack.push_front(new RegInstr(holder));
    }
      break;
    case REG_INT: {
      RegisterHolder* lhs = right->GetRegister();
      RegisterHolder* rhs = GetRegister();
      move_mem_reg(left->GetOperand(), EBP, rhs->GetRegister());
      math_reg_reg(lhs->GetRegister(), rhs->GetRegister(), instruction->GetType());
      ReleaseRegister(lhs);
      working_stack.push_front(new RegInstr(rhs));
    }
      break;
    case MEM_INT: {
      RegisterHolder* holder = GetRegister();
      move_mem_reg(left->GetOperand(), EBP, holder->GetRegister());
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

  jit::InstructionType type = instruction->GetType();
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
      move_mem_xreg(right->GetOperand(), EBP, holder->GetRegister());

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
      RegisterHolder* left_holder = left->GetRegister();
      RegisterHolder* right_holder = GetXmmRegister();
      move_imm_xreg(right, right_holder->GetRegister());
      
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
	move_mem_xreg(right->GetOperand(), EBP, right_holder->GetRegister());
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
    switch(right->GetType()) {
    case IMM_FLOAT: {
      RegisterHolder* holder = GetXmmRegister();
      move_mem_xreg(left->GetOperand(), EBP, holder->GetRegister());
      
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
	move_mem_xreg(left->GetOperand(), EBP, right_holder->GetRegister());
	math_xreg_xreg(holder->GetRegister(), right_holder->GetRegister(), instruction->GetType());
	ReleaseXmmRegister(holder);
	working_stack.push_front(new RegInstr(right_holder));
      }
    }
      break;
      
    case MEM_FLOAT:
    case MEM_INT: {
      RegisterHolder* left_holder = GetXmmRegister();
      move_mem_xreg(left->GetOperand(), EBP, left_holder->GetRegister());

      RegisterHolder* right_holder = GetXmmRegister();
      move_mem_xreg(right->GetOperand(), EBP, right_holder->GetRegister());
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

void JitCompiler::move_reg_reg(Register src, Register dest) {
  if(src != dest) {
#ifdef _DEBUG
    wcout << L"  " << (++instr_count) << L": [movl %" << GetRegisterName(src) 
	 << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
    // encode
    AddMachineCode(0x89);
    unsigned char code = 0xc0;
    // write value
    RegisterEncode3(code, 2, src);
    RegisterEncode3(code, 5, dest);
    AddMachineCode(code);
  }
}

void JitCompiler::move_reg_mem16(Register src, int32_t offset, Register dest) { 
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movw %" << GetRegisterName(src) 
       << L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
       << endl;
#endif
  // encode
  AddMachineCode(0x66);
  AddMachineCode(0x89);
  AddMachineCode(ModRM(dest, src));
  // write value
  AddImm(offset);
}

void JitCompiler::move_reg_mem8(Register src, int32_t offset, Register dest) { 
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movb %" << GetRegisterName(src) 
       << L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
       << endl;
#endif
  // encode
  AddMachineCode(0x88);
  AddMachineCode(ModRM(dest, src));
  // write value
  AddImm(offset);
}
    
void JitCompiler::move_reg_mem(Register src, int32_t offset, Register dest) { 
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movl %" << GetRegisterName(src) 
       << L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
       << endl;
#endif
  // encode
  AddMachineCode(0x89);
  AddMachineCode(ModRM(dest, src));
  // write value
  AddImm(offset);
}

void JitCompiler::move_mem8_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movb " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest)
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x0f);
  AddMachineCode(0xb6);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::move_mem16_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movw " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest)
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x0f);
  AddMachineCode(0xb7);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::move_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movl " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest)
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x8b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}
    
void JitCompiler::move_imm_memx(RegInstr* instr, int32_t offset, Register dest) {
  RegisterHolder* tmp_holder = GetXmmRegister();
  move_imm_xreg(instr, tmp_holder->GetRegister());
  move_xreg_mem(tmp_holder->GetRegister(), offset, dest);
  ReleaseXmmRegister(tmp_holder);
}

void JitCompiler::move_imm_mem8(int32_t imm, int32_t offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movb $" << imm << L", " << offset 
       << L"(%" << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xc6);
  unsigned char code = 0x80;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  // write value
  AddImm(offset);
  AddMachineCode(imm);
}

void JitCompiler::move_imm_mem16(int32_t imm, int32_t offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movw $" << imm << L", " << offset 
       << L"(%" << GetRegisterName(dest) << L")" << L"]" << endl;
#endif

  #ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movl $" << imm << L", " << offset 
       << L"(%" << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x66);    
  AddMachineCode(0xc7);    
  unsigned char code = 0x80;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  // write value
  AddImm(offset);
  AddImm16(imm);
}

void JitCompiler::move_imm_mem(int32_t imm, int32_t offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movl $" << imm << L", " << offset 
       << L"(%" << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xc7);    
  unsigned char code = 0x80;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  // write value
  AddImm(offset);
  AddImm(imm);
}

void JitCompiler::move_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movl $" << imm << L", %" 
       << GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  unsigned char code = 0xb8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::move_imm_xreg(RegInstr* instr, Register reg) {
  // copy address of imm value
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(instr->GetOperand(), imm_holder->GetRegister());  
  move_mem_xreg(0, imm_holder->GetRegister(), reg);
  ReleaseRegister(imm_holder);
}
    
void JitCompiler::move_mem_xreg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movsd " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest)
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(0x0f);
  AddMachineCode(0x10);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}
    
void JitCompiler::move_xreg_mem(Register src, int32_t offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [movsd %" << GetRegisterName(src) 
       << L", " << offset << L"(%" << GetRegisterName(dest) << L")" << L"]" 
       << endl;
#endif 
  // encode
  AddMachineCode(0xf2);
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
    AddMachineCode(0x0f);
    AddMachineCode(0x11);
    unsigned char code = 0xc0;
    // write value
    RegisterEncode3(code, 2, src);
    RegisterEncode3(code, 5, dest);
    AddMachineCode(code);
  }
}

bool JitCompiler::cond_jmp(jit::InstructionType type) {
  /*
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
    jump_table.insert(pair<int32_t, StackInstr*>(code_index, next_instr));
    // temp offset
    AddImm(0);
    skip_jump = true;
    
    return true;
  }
  */

  return false;
}

void JitCompiler::math_imm_reg(int32_t imm, Register reg, jit::InstructionType type) {
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

void JitCompiler::math_reg_reg(Register src, Register dest, jit::InstructionType type) {
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

void JitCompiler::math_mem_reg(int32_t offset, Register reg, jit::InstructionType type) {
  switch(type) {
  case SHL_INT:
    shl_mem_reg(offset, EBP, reg);
    break;

  case SHR_INT:
    shr_mem_reg(offset, EBP, reg);
    break;
    
  case AND_INT:
    and_mem_reg(offset, EBP, reg);
    break;

  case OR_INT:
    or_mem_reg(offset, EBP, reg);
    break;
    
  case ADD_INT:
    add_mem_reg(offset, EBP, reg);
    break;

  case SUB_INT:
    sub_mem_reg(offset, EBP, reg);
    break;

  case MUL_INT:
    mul_mem_reg(offset, EBP, reg);
    break;

  case DIV_INT:
    div_mem_reg(offset, EBP, reg, false);
    break;
    
  case MOD_INT:
    div_mem_reg(offset, EBP, reg, true);
    break;

  case BIT_AND_INT:
    and_mem_reg(offset, EBP, reg);
    break;

  case BIT_OR_INT:
    or_mem_reg(offset, EBP, reg);
    break;

  case BIT_XOR_INT:
    xor_mem_reg(offset, EBP, reg);
    break;
    
  case LES_INT:
  case LES_EQL_INT:
  case GTR_INT:
  case EQL_INT:
  case NEQL_INT:  
  case GTR_EQL_INT:
    cmp_mem_reg(offset, EBP, reg);
    if(!cond_jmp(type)) {
      cmov_reg(reg, type);
    }
    break;

  default:
    break;
  }
}

void JitCompiler::math_imm_xreg(RegInstr* instr, Register reg, jit::InstructionType type) {
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

void JitCompiler::math_mem_xreg(int32_t offset, Register dest, jit::InstructionType type) {
  RegisterHolder* holder = GetXmmRegister();
  move_mem_xreg(offset, EBP, holder->GetRegister());
  math_xreg_xreg(holder->GetRegister(), dest, type);
  ReleaseXmmRegister(holder);
}

void JitCompiler::math_xreg_xreg(Register src, Register dest, jit::InstructionType type) {
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
  wcout << L"  " << (++instr_count) << L": [cmpll %" << GetRegisterName(src) 
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x39);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::cmp_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cmpl " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x3b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}
    
void JitCompiler::cmp_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cmpl $" << imm << L", %"
       << GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x81);
  unsigned char code = 0xf8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::cmov_reg(Register reg, jit::InstructionType oper) {
  // set register to 0; if eflag than set to 1
  move_imm_reg(0, reg);
  RegisterHolder* true_holder = GetRegister();
  move_imm_reg(1, true_holder->GetRegister());
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cmovl %" << GetRegisterName(reg) << L", %" 
       << GetRegisterName(true_holder->GetRegister()) << L" ]" << endl;
#endif
  // encode
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
    wcerr << L">>> Unknown compare! <<<" << endl;
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

void JitCompiler::add_imm_mem(int32_t imm, int32_t offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addl $" << imm << L", " 
       << offset << L"(%"<< GetRegisterName(dest) << L")]" << endl;
#endif
  // encode
  AddMachineCode(0x81);
  AddMachineCode(ModRM(dest, EAX));
  // write value
  AddImm(offset);
  AddImm(imm);
}
    
void JitCompiler::add_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addl $" << imm << L", %"
       << GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
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
  wcout << L"  " << (++instr_count) << L": [addl %" << GetRegisterName(src) 
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
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
  AddMachineCode(0x0f);
  AddMachineCode(0x58);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}
    
void JitCompiler::add_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addl " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x03);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::add_mem_xreg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [addsd " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(0x0f);
  AddMachineCode(0x58);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);  
}

void JitCompiler::sub_mem_xreg(int32_t offset, Register src, Register dest) {
  RegisterHolder* holder = GetXmmRegister();
  move_mem_xreg(offset, src, holder->GetRegister());
  sub_xreg_xreg(dest, holder->GetRegister());
  move_xreg_xreg(holder->GetRegister(), dest);
  ReleaseXmmRegister(holder);
}

void JitCompiler::mul_mem_xreg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [mulsd " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(0x0f);
  AddMachineCode(0x59);
  AddMachineCode(ModRM(src, dest));
  AddImm(offset);
}

void JitCompiler::div_mem_xreg(int32_t offset, Register src, Register dest) {
  RegisterHolder* holder = GetXmmRegister();
  move_mem_xreg(offset, src, holder->GetRegister());
  div_xreg_xreg(dest, holder->GetRegister());
  move_xreg_xreg(holder->GetRegister(), dest);
  ReleaseXmmRegister(holder);
}

void JitCompiler::sub_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subl $" << imm << L", %"
       << GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x81);
  unsigned char code = 0xe8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  AddImm(imm);
}

void JitCompiler::sub_imm_mem(int32_t imm, int32_t offset, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subl $" << imm << L", " 
       << offset << L"(%"<< GetRegisterName(dest) << L")]" << endl;
#endif
  // encode
  AddMachineCode(0x81);
  AddMachineCode(ModRM(dest, EBP));
  // write value
  AddImm(offset);
  AddImm(imm);
}

void JitCompiler::sub_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subl %" << GetRegisterName(src)
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x29);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::sub_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [subl " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif

  // encode
  AddMachineCode(0x2b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::mul_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [imul $" << imm 
       << L", %"<< GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
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
  wcout << L"  " << (++instr_count) << L": [imul %" 
       << GetRegisterName(src) << L", %"<< GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x0f);
  AddMachineCode(0xaf);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::mul_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [imul " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x0f);
  AddMachineCode(0xaf);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::div_imm_reg(int32_t imm, Register reg, bool is_mod) {
  RegisterHolder* imm_holder = GetRegister();
  move_imm_reg(imm, imm_holder->GetRegister());
  div_reg_reg(imm_holder->GetRegister(), reg, is_mod);
  ReleaseRegister(imm_holder);
}

void JitCompiler::div_mem_reg(int32_t offset, Register src,
				  Register dest, bool is_mod) {
  if(is_mod) {
    if(dest != EDX) {
      move_reg_mem(EDX, TMP_REG_1, EBP);
    }
    move_reg_mem(EAX, TMP_REG_0, EBP);
  }
  else {
    if(dest != EAX) {
      move_reg_mem(EAX, TMP_REG_0, EBP);
    }
    move_reg_mem(EDX, TMP_REG_1, EBP);
  }

  // ============
  move_reg_reg(dest, EAX);
  AddMachineCode(0x99); // cdq
  
  // encode
  AddMachineCode(0xf7);
  AddMachineCode(ModRM(src, EDI));
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
    if(dest != EDX) {
      move_reg_reg(EDX, dest);
      move_mem_reg(TMP_REG_1, EBP, EDX);
    }

    if(dest != EAX) {
      move_mem_reg(TMP_REG_0, EBP, EAX);
    }
  }
  else {
    if(dest != EAX) {
      move_reg_reg(EAX, dest);
      move_mem_reg(TMP_REG_0, EBP, EAX);
    }
    
    if(dest != EDX) {
      move_mem_reg(TMP_REG_1, EBP, EDX);
    }
  }
}

void JitCompiler::div_reg_reg(Register src, Register dest, bool is_mod) {
  if(is_mod) {
    if(dest != EDX) {
      move_reg_mem(EDX, TMP_REG_1, EBP);
    }
    move_reg_mem(EAX, TMP_REG_0, EBP);
  }
  else {
    if(dest != EAX) {
      move_reg_mem(EAX, TMP_REG_0, EBP);
    }
    move_reg_mem(EDX, TMP_REG_1, EBP);
  }
  
  // ============
  move_reg_reg(dest, EAX);
  AddMachineCode(0x99); // cdq

  if(src != EAX && src != EDX) {
    // encode
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
    AddMachineCode(0xf7);
    AddMachineCode(ModRM(EBP, EDI));
    // write value
    if(src == EAX) {
      AddImm(TMP_REG_0);
    }
    else {
      AddImm(TMP_REG_1);
    }
    
#ifdef _DEBUG
    if(is_mod) {
      wcout << L"  " << (++instr_count) << L": [imod " << TMP_REG_0 << L"(%" 
	   << GetRegisterName(EBP) << L")]" << endl;
    }
    else {
      wcout << L"  " << (++instr_count) << L": [idiv " << TMP_REG_0 << L"(%" 
	   << GetRegisterName(EBP) << L")]" << endl;
    }
#endif
  }
  // ============
  
  if(is_mod) {
    if(dest != EDX) {
      move_reg_reg(EDX, dest);
      move_mem_reg(TMP_REG_1, EBP, EDX);
    }

    if(dest != EAX) {
      move_mem_reg(TMP_REG_0, EBP, EAX);
    }
  }
  else {
    if(dest != EAX) {
      move_reg_reg(EAX, dest);
      move_mem_reg(TMP_REG_0, EBP, EAX);
    }
    
    if(dest != EDX) {
      move_mem_reg(TMP_REG_1, EBP, EDX);
    }
  }
}

void JitCompiler::dec_reg(Register dest) {
  unsigned char code = 0x48;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [decl %" 
       << GetRegisterName(dest) << L"]" << endl;
#endif
}

void JitCompiler::dec_mem(int32_t offset, Register dest) {
  AddMachineCode(0xff);
  unsigned char code = 0x88;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddImm(offset);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [decl " << offset << L"(%" 
       << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
}

void JitCompiler::inc_mem(int32_t offset, Register dest) {
  AddMachineCode(0xff);
  unsigned char code = 0x80;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddImm(offset);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [incl " << offset << L"(%" 
       << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
}

void JitCompiler::shl_imm_reg(int32_t value, Register dest) {
  AddMachineCode(0xc1);
  unsigned char code = 0xe0;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddMachineCode(value);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shl $" << value << L", %" 
       << GetRegisterName(dest) << L"]" << endl;
#endif
}

void JitCompiler::shl_reg_reg(Register src, Register dest)
{
  Register old_dest;
  RegisterHolder* reg_holder = NULL;
  if(dest == ECX) {
    reg_holder = GetRegister();
    old_dest = dest;
    dest = reg_holder->GetRegister();
    move_reg_reg(old_dest, dest);
  }
  
  if(src != ECX) {
    move_reg_mem(ECX, TMP_REG_0, EBP);
    move_reg_reg(src, ECX);
  }
  
  // --------------------

  // encode
  AddMachineCode(0xd3);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, ESP);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shl %" << GetRegisterName(src) 
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif

  // --------------------
  
  if(src != ECX) {
    move_mem_reg(TMP_REG_0, EBP, ECX);
  }
  
  if(reg_holder) {
    move_reg_reg(dest, old_dest);
    ReleaseRegister(reg_holder);
  }
}

void JitCompiler::shl_mem_reg(int32_t offset, Register src, Register dest) 
{
  RegisterHolder* mem_holder = GetRegister();
  move_mem_reg(offset, src, mem_holder->GetRegister());
  shl_reg_reg(mem_holder->GetRegister(), dest);
  ReleaseRegister(mem_holder);
}

void JitCompiler::shr_imm_reg(int32_t value, Register dest) {
  AddMachineCode(0xc1);
  unsigned char code = 0xe8;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddMachineCode(value);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shr $" << value << L", %" 
       << GetRegisterName(dest) << L"]" << endl;
#endif
}

void JitCompiler::shr_reg_reg(Register src, Register dest)
{
  Register old_dest;
  RegisterHolder* reg_holder = NULL;
  if(dest == ECX) {
    reg_holder = GetRegister();
    old_dest = dest;
    dest = reg_holder->GetRegister();
    move_reg_reg(old_dest, dest);
  }
  
  if(src != ECX) {
    move_reg_mem(ECX, TMP_REG_0, EBP);
    move_reg_reg(src, ECX);
  }
  
  // --------------------
  
  // encode
  AddMachineCode(0xd3);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, EBP);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [shr %" << GetRegisterName(ECX) 
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif

  // --------------------
  
  if(src != ECX) {
    move_mem_reg(TMP_REG_0, EBP, ECX);
  }
  
  if(reg_holder) {
    move_reg_reg(dest, old_dest);
    ReleaseRegister(reg_holder);
  }
}

void JitCompiler::shr_mem_reg(int32_t offset, Register src, Register dest) 
{
  RegisterHolder* mem_holder = GetRegister();
  move_mem_reg(offset, src, mem_holder->GetRegister());
  shr_reg_reg(mem_holder->GetRegister(), dest);
  ReleaseRegister(mem_holder);
}

void JitCompiler::push_mem(int32_t offset, Register dest) {
  AddMachineCode(0xff);
  unsigned char code = 0xb0;
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
  AddImm(offset);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [pushl " << offset << L"(%" 
       << GetRegisterName(dest) << L")" << L"]" << endl;
#endif
}

void JitCompiler::push_reg(Register reg) {
  unsigned char code = 0x50;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [pushl %" << GetRegisterName(reg) 
       << L"]" << endl;
#endif
}

void JitCompiler::push_imm(int32_t value) {
  AddMachineCode(0x68);
  AddImm(value);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [pushl $" << value << L"]" << endl;
#endif
}

void JitCompiler::pop_reg(Register reg) {
  unsigned char code = 0x58;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [popl %" << GetRegisterName(reg) 
       << L"]" << endl;
#endif
}

void JitCompiler::call_reg(Register reg) {
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
  AddMachineCode(0x0f);
  AddMachineCode(0x2e);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, dest);
  RegisterEncode3(code, 5, src);
  AddMachineCode(code);
}

void JitCompiler::cmp_mem_xreg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [ucomisd " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x66);
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

void JitCompiler::round_mem_xreg(int32_t offset, Register src, Register dest, bool is_floor) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << (is_floor ? ": [floor " : ": [ceil ") 
       << offset << L"(%" << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  
  AddMachineCode(0x66);
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

void JitCompiler::cvt_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cvtsd2si " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
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

void JitCompiler::cvt_mem_xreg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [cvtsi2sd " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0xf2);
  AddMachineCode(0x0f);
  AddMachineCode(0x2a);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::and_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [andl $" << imm << L", %"
       << GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x81);
  unsigned char code = 0xe0;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::and_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [andl %" << GetRegisterName(src) 
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x21);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::and_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [andl " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x23);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::or_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [orl $" << imm << L", %"
       << GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x81);
  unsigned char code = 0xc8;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}


void JitCompiler::or_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [orl %" << GetRegisterName(src) 
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x09);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::or_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [orl " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x0b);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}

void JitCompiler::xor_imm_reg(int32_t imm, Register reg) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [xorl $" << imm << L", %"
       << GetRegisterName(reg) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x81);
  unsigned char code = 0xf0;
  RegisterEncode3(code, 5, reg);
  AddMachineCode(code);
  // write value
  AddImm(imm);
}

void JitCompiler::xor_reg_reg(Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [xor %" << GetRegisterName(src) 
       << L", %" << GetRegisterName(dest) << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x31);
  unsigned char code = 0xc0;
  // write value
  RegisterEncode3(code, 2, src);
  RegisterEncode3(code, 5, dest);
  AddMachineCode(code);
}

void JitCompiler::xor_mem_reg(int32_t offset, Register src, Register dest) {
#ifdef _DEBUG
  wcout << L"  " << (++instr_count) << L": [xorl " << offset << L"(%" 
       << GetRegisterName(src) << L"), %" << GetRegisterName(dest) 
       << L"]" << endl;
#endif
  // encode
  AddMachineCode(0x33);
  AddMachineCode(ModRM(src, dest));
  // write value
  AddImm(offset);
}