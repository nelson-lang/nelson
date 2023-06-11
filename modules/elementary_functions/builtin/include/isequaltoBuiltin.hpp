//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::ElementaryFunctionsGateway {
ArrayOfVector
isequaltoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
struct_isequaltoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
cell_isequaltoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
char_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
logical_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
double_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
single_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int8_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int16_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int32_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int64_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint8_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint16_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint32_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint64_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
string_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
