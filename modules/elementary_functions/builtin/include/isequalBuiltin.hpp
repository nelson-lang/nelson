//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::ElementaryFunctionsGateway {
ArrayOfVector
isequalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
struct_isequalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
cell_isequalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
char_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
logical_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
double_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
single_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int8_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int16_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int32_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int64_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint8_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint16_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint32_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint64_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
string_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
