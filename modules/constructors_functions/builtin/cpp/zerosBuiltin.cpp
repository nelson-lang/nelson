//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zerosBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Zeros.hpp"
#include "StringToClass.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::zerosBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    NelsonType cl = NLS_DOUBLE;
    if (argIn.empty()) {
        retval << Zeros(cl);
    } else {
        sizeType nRhs = argIn.size();
        bool bCheckClassName = true;
        if (static_cast<int>(nRhs) - 2 >= 0) {
            ArrayOf Arg = argIn[argIn.size() - 2];
            if (Arg.isRowVectorCharacterArray() || Arg.isScalarStringArray()) {
                std::wstring paramstr = Arg.getContentAsWideString();
                if (paramstr == L"like") {
                    ArrayOf lastArg = argIn[argIn.size() - 1];
                    if (lastArg.isSparse()) {
                        raiseError(L"Nelson:constructors_functions:ERROR_SUPPORTED_TYPE_EXPECTED_"
                                   L"LAST_ARGUMENT",
                            ERROR_SUPPORTED_TYPE_EXPECTED_LAST_ARGUMENT);
                    }
                    switch (lastArg.getDataClass()) {
                    case NLS_LOGICAL:
                        cl = NLS_LOGICAL;
                        break;
                    case NLS_INT8:
                        cl = NLS_INT8;
                        break;
                    case NLS_UINT8:
                        cl = NLS_UINT8;
                        break;
                    case NLS_INT16:
                        cl = NLS_INT16;
                        break;
                    case NLS_UINT16:
                        cl = NLS_UINT16;
                        break;
                    case NLS_INT32:
                        cl = NLS_INT32;
                        break;
                    case NLS_UINT32:
                        cl = NLS_UINT32;
                        break;
                    case NLS_INT64:
                        cl = NLS_INT64;
                        break;
                    case NLS_UINT64:
                        cl = NLS_UINT64;
                        break;
                    case NLS_DOUBLE:
                        cl = NLS_DOUBLE;
                        break;
                    case NLS_SINGLE:
                        cl = NLS_SINGLE;
                        break;
                    case NLS_DCOMPLEX:
                        cl = NLS_DCOMPLEX;
                        break;
                    case NLS_SCOMPLEX:
                        cl = NLS_SCOMPLEX;
                        break;
                    default:
                        raiseError(L"Nelson:constructors_functions:ERROR_SUPPORTED_TYPE_EXPECTED_"
                                   L"LAST_ARGUMENT",
                            ERROR_SUPPORTED_TYPE_EXPECTED_LAST_ARGUMENT);
                        break;
                    }
                    nRhs = nRhs - 2;
                    bCheckClassName = false;
                } else {
                    raiseError(L"Nelson:constructors_functions:ERROR_LIKE_EXPECTED_AT_N_MINUS_2",
                        ERROR_LIKE_EXPECTED_AT_N_MINUS_2);
                }
            }
        }
        ArrayOf lastArg = argIn[argIn.size() - 1];
        if (lastArg.isRowVectorCharacterArray() && bCheckClassName) {
            cl = StringToClass(lastArg.getContentAsCString());
            nRhs--;
            if (cl > NLS_LOGICAL) {
                raiseError(
                    L"Nelson:constructors_functions:ERROR_SUPPORTED_TYPE_EXPECTED_LAST_ARGUMENT",
                    ERROR_SUPPORTED_TYPE_EXPECTED_LAST_ARGUMENT);
            }
        }
        if (nRhs == 0) {
            retval << Zeros(cl);
            return retval;
        }
        Dimensions dims;
        if (nRhs == 1) {
            if (argIn[0].isNumeric() && !argIn[0].isSparse()) {
                if (argIn[0].isRowVector()) {
                    if (argIn[0].isEmpty()) {
                        raiseError(L"Nelson:constructors_functions:ERROR_WRONG_ARGUMENT_X_SIZE_ROW_"
                                   L"VECTOR_EXPECTED",
                            ERROR_WRONG_ARGUMENT_X_SIZE_ROW_VECTOR_EXPECTED, 1);
                    }
                    if (argIn[0].getElementCount() < Nelson::maxDims) {
                        ArrayOf dimVector(argIn[0]);
                        auto* ptrValues
                            = static_cast<indexType*>(dimVector.getContentAsIndexPointer());
                        ompIndexType elementCount = dimVector.getElementCount();
                        for (ompIndexType k = 0; k < elementCount; k++) {
                            dims[k] = static_cast<indexType>(ptrValues[k]);
                        }
                        if (dims.getLength() == 1) {
                            dims[1] = dims[0];
                        }
                    } else {
                        raiseError(L"Nelson:constructors_functions:ERROR_TOO_MANY_DIMENSIONS_"
                                   L"CURRENT_LIMIT",
                            ERROR_TOO_MANY_DIMENSIONS_CURRENT_LIMIT,
                            std::to_wstring(Nelson::maxDims));
                    }
                } else {
                    raiseError(L"Nelson:constructors_functions:ERROR_WRONG_ARGUMENT_X_SIZE_ROW_"
                               L"VECTOR_EXPECTED",
                        ERROR_WRONG_ARGUMENT_X_SIZE_ROW_VECTOR_EXPECTED, 1);
                }
            } else {
                raiseError(
                    L"Nelson:constructors_functions:ERROR_WRONG_ARGUMENT_X_TYPE_NUMERIC_EXPECTED",
                    ERROR_WRONG_ARGUMENT_X_TYPE_NUMERIC_EXPECTED, 1);
            }
        } else {
            for (sizeType k = 0; k < nRhs; k++) {
                dims[k] = argIn[k].getContentAsScalarIndex(true, true, true);
            }
            if (dims.getLength() == 1) {
                dims[1] = dims[0];
            }
        }
        retval << Zeros(dims, cl);
    }
    return retval;
}
//=============================================================================
