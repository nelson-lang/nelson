//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "generic_colonBuiltin.hpp"
#include "Error.hpp"
#include "Colon.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson::OperatorsGateway {
//=============================================================================
ArrayOfVector
generic_colonBuiltin(NelsonType nlsType, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf res;
    if (argIn.size() == 2) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        if (nlsType >= NLS_INT8 && nlsType <= NLS_UINT64) {
            if (A.isDoubleType()) {
                double d = A.getContentAsDoubleScalar(true);
                if (int64(d) != d) {
                    Error(_W("Colon input arguments must have same type."));
                }
            }
            if (B.isDoubleType()) {
                double d = B.getContentAsDoubleScalar(true);
                if (int64(d) != d) {
                    Error(_W("Colon input arguments must have same type."));
                }
            }
        }
        A.promoteType(nlsType);
        B.promoteType(nlsType);
        res = Colon(A, B);
    } else if (argIn.size() == 3) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        ArrayOf C = argIn[2];
        A.promoteType(nlsType);
        B.promoteType(nlsType);
        C.promoteType(nlsType);
        if (nlsType >= NLS_INT8 && nlsType <= NLS_UINT64) {
            if (A.isDoubleType()) {
                double d = A.getContentAsDoubleScalar(true);
                if (int64(d) != d) {
                    Error(_W("Colon input arguments must have same type."));
                }
            }
            if (B.isDoubleType()) {
                double d = B.getContentAsDoubleScalar(true);
                if (int64(d) != d) {
                    Error(_W("Colon input arguments must have same type."));
                }
            }
            if (C.isDoubleType()) {
                double d = C.getContentAsDoubleScalar(true);
                if (int64(d) != d) {
                    Error(_W("Colon input arguments must have same type."));
                }
            }
        }
        res = Colon(A, B, C);
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    retval << res;
    return retval;
}
//=============================================================================
ArrayOfVector
double_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_DOUBLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
single_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_SINGLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
uint8_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_UINT8, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
uint16_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_UINT16, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
uint32_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_UINT32, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
uint64_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_UINT64, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
int8_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_INT8, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
int16_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_INT16, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
int32_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_INT32, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
int64_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_INT64, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
char_colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_colonBuiltin(NLS_CHAR, nLhs, argIn);
}
//=============================================================================
}
//=============================================================================
