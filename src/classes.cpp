#include "classes.h"

using namespace jit;

/****************************
 * Integer class
 ****************************/
IntegerClass* IntegerClass::instance;

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

void IntegerClass::Subtract(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = INT_VALUE;
    result.klass = right.klass;
    result.value.int_value = left.value.int_value - right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(SUB_INT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.klass = right.klass;
    result.value.float_value = left.value.int_value - right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(SUB_FLOAT));
    }
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
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(MUL_INT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.klass = right.klass;
    result.value.float_value = left.value.int_value * right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(MUL_FLOAT));
    }
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::Divide(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = INT_VALUE;
    result.klass = right.klass;
    result.value.int_value = left.value.int_value / right.value.int_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(DIV_INT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.klass = right.klass;
    result.value.float_value = left.value.int_value / right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(DIV_FLOAT));
    }
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
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(GTR_FLOAT));
    }
    break;

  default:
    wcerr << L">>> invalid logical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::Equal(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value == right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(EQL_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value == right.value.float_value;
		if(is_recording) {
      jit_instrs.push_back(new JitInstruction(EQL_FLOAT));
    } 
    break;

  default:
    wcerr << L">>> invalid logical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::NotEqual(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value != right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.int_value != right.value.float_value;
		if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_FLOAT));
    } 
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

void FloatClass::Add(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value + right.value.int_value;
		// record JIT instructions
    if(is_recording) {
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

void FloatClass::Subtract(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value - right.value.int_value;
		// record JIT instructions
    if(is_recording) {
			jit_instrs.push_back(new JitInstruction(SUB_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value - right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(SUB_FLOAT));
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
		// record JIT instructions
    if(is_recording) {
			jit_instrs.push_back(new JitInstruction(MUL_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value * right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(MUL_FLOAT));
    }
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void FloatClass::Divide(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value - right.value.int_value;
		// record JIT instructions
    if(is_recording) {
			jit_instrs.push_back(new JitInstruction(DIV_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = FLOAT_VALUE;
    result.value.float_value = left.value.float_value - right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(DIV_FLOAT));
    }
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
		// record JIT instructions
    if(is_recording) {
		  jit_instrs.push_back(new JitInstruction(LES_FLOAT));
    }
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
		// record JIT instructions
    if(is_recording) {
			jit_instrs.push_back(new JitInstruction(GTR_FLOAT));
    }
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

void FloatClass::Equal(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value == right.value.int_value;
		// record JIT instructions
    if(is_recording) {
		  jit_instrs.push_back(new JitInstruction(EQL_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value == right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(EQL_FLOAT));
    }
    break;
		
  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void FloatClass::NotEqual(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value != right.value.int_value;
		// record JIT instructions
    if(is_recording) {
		  jit_instrs.push_back(new JitInstruction(NEQL_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.value.int_value = left.value.float_value != right.value.float_value;
		// record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_FLOAT));
    }
    break;
		
  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}
