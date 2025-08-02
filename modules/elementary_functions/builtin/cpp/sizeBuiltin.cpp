//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sizeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::sizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bChooseDimension = false;
    std::vector<indexType> dimsVal;
    switch (argIn.size()) {
    case 1: {
        bChooseDimension = false;
    } break;
    case 2: {
        bChooseDimension = true;
        ArrayOf param1(argIn[1]);
        if (param1.isRowVectorCharacterArray()) {
            std::wstring str = param1.getContentAsWideString();
            if ((str == L"r") || (str == L"c")) {
                if (str == L"r") {
                    dimsVal.push_back(1);
                } else {
                    dimsVal.push_back(2);
                }
            } else {
                Error(_W("Wrong value for argument #2. 'r' or 'c' expected"));
            }
        } else {
            if (param1.isScalar()) {
                dimsVal.push_back(param1.getContentAsScalarIndex(false));
            } else {
                if (param1.isRowVector()) {
                    if (param1.isNumeric()) {
                        indexType* values = (indexType*)param1.getContentAsIndexPointer();
                        indexType elementCount = param1.getElementCount();
                        for (indexType k = 0; k < elementCount; k++) {
                            dimsVal.push_back(values[k]);
                        }
                    } else {
                        Error(_W("Wrong type for argument #2. numeric values expected"));
                    }
                } else {
                    Error(_W("Wrong size for argument #2. row vector or scalar expected"));
                }
            }
        }
    } break;
    default:
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        break;
    }
    ArrayOf param1 = argIn[0];
    Dimensions sze(param1.getDimensions());
    sze.simplify();
    if (bChooseDimension) {
        nargoutcheck(nLhs, 0, 1);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsVal.size());
        Dimensions outDims(1, dimsVal.size());
        ArrayOf res = ArrayOf(NLS_DOUBLE, outDims, ptr);
        for (indexType k = 0; k < (indexType)dimsVal.size(); k++) {
            if (dimsVal[k] - 1 >= maxDims) {
                ptr[k] = 1.0;
            } else {
                ptr[k] = (double)(sze[dimsVal[k] - 1]);
            }
        }
        retval << res;
    } else {
        if (nLhs > 1) {
            for (int i = 0; i < nLhs; i++) {
                retval << ArrayOf::doubleConstructor(static_cast<double>(sze[i]));
            }
        } else {
            double* dims = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, sze.getLength(), stringVector(), true));
            Dimensions retDim(2);
            if (sze.getLength() == 0) {
                retDim[0] = 0;
                retDim[1] = 0;
            } else {
                for (indexType i = 0; i < sze.getLength(); i++) {
                    dims[i] = static_cast<double>(sze[i]);
                }
                retDim[0] = 1;
                retDim[1] = sze.getLength();
            }
            retval << ArrayOf(NLS_DOUBLE, retDim, dims);
        }
    }
    return retval;
}
//=============================================================================
