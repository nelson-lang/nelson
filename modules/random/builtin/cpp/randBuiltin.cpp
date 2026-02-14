//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "randBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Rand.hpp"
#include "nlsBuildConfig.h"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::randBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    NelsonType cl = NLS_DOUBLE;
    if (argIn.size() == 0) {
        retval << Rand(cl);
    } else {
        sizeType nRhs = argIn.size();
        bool bCheckClassName = true;
        if ((int)nRhs - 2 >= 0) {
            ArrayOf Arg = argIn[argIn.size() - 2];
            if (Arg.isRowVectorCharacterArray() || Arg.isScalarStringArray()) {
                std::wstring paramstr = Arg.getContentAsWideString();
                if (paramstr == L"like") {
                    ArrayOf lastArg = argIn[argIn.size() - 1];
                    switch (lastArg.getDataClass()) {
                    case NLS_SCOMPLEX:
                        cl = NLS_SCOMPLEX;
                        break;
                    case NLS_DCOMPLEX:
                        cl = NLS_DCOMPLEX;
                        break;
                    case NLS_DOUBLE:
                        cl = NLS_DOUBLE;
                        break;
                    case NLS_SINGLE:
                        cl = NLS_SINGLE;
                        break;
                    default:
                        raiseError(
                            L"Nelson:random:ERROR_SINGLE_OR_DOUBLE_EXPECTED_AT_LAST_ARGUMENT",
                            ERROR_SINGLE_OR_DOUBLE_EXPECTED_AT_LAST_ARGUMENT);
                        break;
                    }
                    nRhs = nRhs - 2;
                    bCheckClassName = false;
                } else {
                    raiseError(L"Nelson:random:ERROR_LIKE_EXPECTED_AT_N_MINUS_2_ARG",
                        ERROR_LIKE_EXPECTED_AT_N_MINUS_2_ARG);
                }
            }
        }
        ArrayOf lastArg = argIn[argIn.size() - 1];
        if (lastArg.isRowVectorCharacterArray() && bCheckClassName) {
            std::wstring paramstr = lastArg.getContentAsWideString();
            if (paramstr == L"double") {
                cl = NLS_DOUBLE;
                nRhs--;
            } else if (paramstr == L"single") {
                cl = NLS_SINGLE;
                nRhs--;
            } else {
                raiseError(L"Nelson:random:ERROR_SINGLE_OR_DOUBLE_EXPECTED_AT_LAST_ARGUMENT",
                    ERROR_SINGLE_OR_DOUBLE_EXPECTED_AT_LAST_ARGUMENT);
            }
        }
        if (nRhs == 0) {
            retval << Rand(cl);
            return retval;
        }
        Dimensions dims;
        if (nRhs == 1) {
            if (argIn[0].isNumeric() && !argIn[0].isSparse()) {
                if (argIn[0].isRowVector()) {
                    if (argIn[0].isEmpty()) {
                        raiseError(L"Nelson:random:ERROR_WRONG_ARGUMENT_X_SIZE_ROW_VECTOR_EXPECTED",
                            ERROR_WRONG_ARGUMENT_X_SIZE_ROW_VECTOR_EXPECTED, 1);
                    }
                    if (argIn[0].getElementCount() < Nelson::maxDims) {
                        ArrayOf dimVector = argIn[0];
                        dimVector.promoteType(NLS_DOUBLE);
                        double* ptrValues = (double*)dimVector.getDataPointer();
                        ompIndexType elementCount = argIn[0].getElementCount();
                        for (ompIndexType k = 0; k < elementCount; k++) {
                            if (ptrValues[k] > 0) {
                                dims[k] = (indexType)ptrValues[k];
                            } else {
                                dims[k] = 0;
                            }
                        }
                        if (dims.getLength() == 1) {
                            dims[1] = dims[0];
                        }
                    } else {
                        raiseError(L"Nelson:random:ERROR_TOO_MANY_DIMENSIONS_CURRENT_LIMIT",
                            ERROR_TOO_MANY_DIMENSIONS_CURRENT_LIMIT, Nelson::maxDims);
                    }
                } else {
                    raiseError(L"Nelson:random:ERROR_WRONG_ARGUMENT_X_SIZE_ROW_VECTOR_EXPECTED",
                        ERROR_WRONG_ARGUMENT_X_SIZE_ROW_VECTOR_EXPECTED, 1);
                }
            } else {
                raiseError(L"Nelson:error_manager:wrong_type_NUMERIC_EXPECTED",
                    ERROR_WRONG_ARGUMENT_X_TYPE_NUMERIC_EXPECTED, 1);
            }
        } else {
            for (sizeType k = 0; k < nRhs; k++) {
                ArrayOf param = argIn[k];
                indexType idx = param.getContentAsScalarIndex();
                dims[k] = idx;
            }
            if (dims.getLength() == 1) {
                dims[1] = dims[0];
            }
        }
        retval << Rand(dims, cl);
    }
    return retval;
}
//=============================================================================
