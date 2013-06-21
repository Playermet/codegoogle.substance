#include "classes.h"

using namespace jit;

/****************************
 * Integer class
 ****************************/
IntegerClass* IntegerClass::instance;

IntegerClass::IntegerClass() {
  AddOperation(L"+", Add);
  AddOperation(L"*", Multiply);
  AddOperation(L"<", Less);
	AddOperation(L">", Greater);
}

IntegerClass::~IntegerClass() {
}

void IntegerClass::Add(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = INT_VALUE;
    result.klass = right.klass;
    result.value.int_value = left.value.int_value + right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(ADD_INT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.klass = right.klass;
    result.value.float_value = left.value.int_value + right.value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::Multiply(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = INT_VALUE;
    result.klass = right.klass;
    result.value.int_value = left.value.int_value * right.value.int_value;
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.klass = right.klass;
    result.value.float_value = left.value.int_value * right.value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::Less(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value < right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(LES_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value < right.value.float_value;
		if(is_recording) {
			jit_instrs.push_back(new JitInstruction(I2F));
      jit_instrs.push_back(new JitInstruction(LES_FLOAT));
    } 
    break;

  default:
    wcerr << L">>> invalid logical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::Greater(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value > right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(GTR_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value > right.value.float_value;
    break;

  default:
    wcerr << L">>> invalid logical operation <<<" << endl;
    exit(1);
    break;
  }
}

/****************************
 * Float class
 ****************************/
FloatClass* FloatClass::instance;

FloatClass::FloatClass() {
  AddOperation(L"+", Add);
  AddOperation(L"*", Multiply);
  AddOperation(L"<", Less);
	AddOperation(L">", Greater);
}

FloatClass::~FloatClass() {
}

void FloatClass::Add(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value + right.value.int_value;
		// record JIT instructions
    if(is_recording) {
			JitInstruction* temp = jit_instrs.back();
			jit_instrs.pop_back();
			jit_instrs.push_back(new JitInstruction(I2F));
			jit_instrs.push_back(temp);
      jit_instrs.push_back(new JitInstruction(ADD_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value + right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(ADD_FLOAT));
    }
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void FloatClass::Multiply(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value * right.value.int_value;
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value * right.value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void FloatClass::Less(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value < right.value.int_value;		
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value < right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(LES_FLOAT));
    }
    break;
		
  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void FloatClass::Greater(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value > right.value.int_value;		
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value > right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(GTR_FLOAT));
    }
    break;
		
  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}
