#include "class.h"

/****************************
 * Integer class
 ****************************/
IntegerClass* IntegerClass::instance;

IntegerClass::IntegerClass() {
  AddOperation(L"+", Add);
  AddOperation(L"*", Multiply);
  AddOperation(L"<", Less);
}

IntegerClass::~IntegerClass() {
}

void IntegerClass::Add(Value* left, Value* right, Value* result) {
  switch(right->type) {
  case INT_VALUE:
    result->type = INT_VALUE;
    result->klass = right->klass;
    result->value.int_value = left->value.int_value + right->value.int_value;
    break;

  case FLOAT_VALUE:
    result->type = FLOAT_VALUE;
    result->klass = right->klass;
    result->value.float_value = left->value.int_value + right->value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::Multiply(Value* left, Value* right, Value* result) {
  switch(right->type) {
  case INT_VALUE:
    result->type = INT_VALUE;
    result->klass = right->klass;
    result->value.int_value = left->value.int_value * right->value.int_value;
    break;

  case FLOAT_VALUE:
    result->type = FLOAT_VALUE;
    result->klass = right->klass;
    result->value.float_value = left->value.int_value * right->value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void IntegerClass::Less(Value* left, Value* right, Value* result) {
  switch(right->type) {
  case INT_VALUE:
    result->type = BOOL_VALUE;
    result->value.int_value = left->value.int_value < right->value.int_value;
    break;

  case FLOAT_VALUE:
    result->type = BOOL_VALUE;
    result->value.float_value = left->value.int_value < right->value.float_value;
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
}

FloatClass::~FloatClass() {
}

void FloatClass::Add(Value* left, Value* right, Value* result) {
  switch(right->type) {
  case INT_VALUE:
    result->type = FLOAT_VALUE;
    result->value.float_value = left->value.float_value + right->value.int_value;
    break;

  case FLOAT_VALUE:
    result->type = FLOAT_VALUE;
    result->value.float_value = left->value.float_value + right->value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void FloatClass::Multiply(Value* left, Value* right, Value* result) {
  switch(right->type) {
  case INT_VALUE:
    result->type = FLOAT_VALUE;
    result->value.float_value = left->value.float_value * right->value.int_value;
    break;

  case FLOAT_VALUE:
    result->type = FLOAT_VALUE;
    result->value.float_value = left->value.float_value * right->value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}

void FloatClass::Less(Value* left, Value* right, Value* result) {
  switch(right->type) {
  case INT_VALUE:
    result->type = BOOL_VALUE;
    result->value.float_value = left->value.float_value < right->value.int_value;
    break;

  case FLOAT_VALUE:
    result->type = BOOL_VALUE;
    result->value.float_value = left->value.float_value < right->value.float_value;
    break;

  default:
    wcerr << L">>> invalid mathematical operation <<<" << endl;
    exit(1);
    break;
  }
}