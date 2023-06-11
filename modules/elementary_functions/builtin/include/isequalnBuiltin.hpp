//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::ElementaryFunctionsGateway {
ArrayOfVector
isequalnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
struct_isequalnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
cell_isequalnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
char_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
logical_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
double_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
single_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int8_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int16_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int32_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int64_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint8_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint16_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint32_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint64_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
string_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
