#include "classes.h"

using namespace jit;

/****************************
* Boolean class
****************************/
BooleanClass* BooleanClass::instance;

void BooleanClass::Equal(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value == right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(EQL_INT));
    }
    break;

  default:
    wcerr << L">>> invalid logical operation <<<" << endl;
    exit(1);
    break;
  }
}

void BooleanClass::NotEqual(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value != right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(EQL_INT));
    }
    break;

  default:
    wcerr << L">>> invalid logical operation <<<" << endl;
    exit(1);
    break;
  }
}

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

void IntegerClass::Modulo(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = INT_VALUE;
    result.klass = right.klass;
    result.value.int_value = left.value.int_value % right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(DIV_INT));
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
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value < right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(LES_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value > right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(GTR_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value == right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(EQL_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value != right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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

void IntegerClass::LessEqual(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value <= right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value <= right.value.float_value;
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

void IntegerClass::GreaterEqual(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value >= right.value.int_value;
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_INT));
    }    
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.int_value >= right.value.float_value;
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

// methods
void IntegerClass::Abs(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count) {
  
}

void IntegerClass::ToFloat(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count) {
  
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

void FloatClass::Modulo(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  wcerr << L">>> invalid mathematical operation <<<" << endl;
  exit(1);
}

void FloatClass::Less(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value < right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(LES_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value > right.value.int_value;	
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(GTR_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value == right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(EQL_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value != right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
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

void FloatClass::LessEqual(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value <= right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value <= right.value.float_value;
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

void FloatClass::GreaterEqual(Value &left, Value &right, Value &result, vector<JitInstruction*> &jit_instrs, bool is_recording) {
  switch(right.type) {
  case INT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value >= right.value.int_value;
    // record JIT instructions
    if(is_recording) {
      jit_instrs.push_back(new JitInstruction(NEQL_FLOAT));
    }
    break;

  case FLOAT_VALUE:
    result.type = BOOL_VALUE;
    result.klass = BooleanClass::Instance();
    result.value.int_value = left.value.float_value >= right.value.float_value;
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

void FloatClass::ToInteger(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count) {
  Value left;
  left.type = INT_VALUE;
  left.klass = IntegerClass::Instance();
  left.value.int_value = (INT_T)self.value.float_value;
#ifdef _DEBUG
  wcout << L"Integer->ToInteger()" << endl;
#endif
  execution_stack[execution_stack_pos++] = left;
}

/****************************
* Array class
****************************/
ArrayClass* ArrayClass::instance;

void ArrayClass::New(Value &self, Value* execution_stack, size_t &execution_stack_pos, INT_T arg_count) {
  if(execution_stack_pos == 0) {
    wcerr << L">>> Array size not specified" << endl;
    exit(1);
  }

  // calculate array size
  size_t array_size = 1;
  vector<INT_T> dimensions;
  for(INT_T i = 0; i < arg_count; i++) {
    Value value = execution_stack[--execution_stack_pos];
    switch(value.type) {      
    case INT_VALUE:
      array_size *= value.value.int_value;
      dimensions.push_back(value.value.int_value);
      break;

    case FLOAT_VALUE:
      array_size *= (INT_T)value.value.float_value;
      dimensions.push_back((INT_T)value.value.float_value);
      break;

    default:
      wcerr << L">>> Operation requires Integer or Float type <<<" << endl;
      exit(1);
    }
  }

  // allocate array
  // layout: [data_offset (0)][max_size (1)][dimensions (2-n)][<- data -> (data_offset - n)]
  const size_t meta_offset = (arg_count + 2);
  const size_t meta_size = sizeof(INT_T) * meta_offset;
  const size_t data_size = sizeof(Value) * array_size;
  void* memory = calloc(meta_size + data_size, 1);

  // set metadata
  INT_T* meta_ptr = (INT_T*)memory;
  meta_ptr[0] = (INT_T)meta_offset;
  meta_ptr[1] = (INT_T)array_size;
  for(size_t i = 0; i < dimensions.size(); i++) {
    meta_ptr[i + 2] = (INT_T)dimensions[i];
  }
#ifdef _DEBUG
  wcout << L"  Array->New" << L"[" << array_size << L"], address=" << memory << endl;
#endif

  // set value
  Value left;
  left.type = ARY_VALUE;
  left.klass = ArrayClass::Instance();
  left.value.ptr_value = memory;
  execution_stack[execution_stack_pos++] = left;
}

/****************************
* Classes class
****************************/
Classes* Classes::instance;