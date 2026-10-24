//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "str2doubleBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "StringToDoubleComplex.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOfView.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::str2doubleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isCharacterArray() || param1.isStringArray() || param1.isCell()) {
        if (param1.isCharacterArray()) {
            std::wstring str = argIn[0].asCharacterArrayWideString();
            bool wasConverted = false;
            doublecomplex value = stringToDoubleComplex(str, wasConverted);
            ArrayOf output = ArrayOf::dsingleComplexScalar(value.real(), value.imag());
            if (output.allReal()) {
                output.promoteType(NLS_DOUBLE);
            }
            retval << output;
            return retval;
        }
        if (param1.isStringArray() || param1.isCell()) {
            Dimensions dimParam1 = param1.dimensions();
            Dimensions dimOutput(dimParam1);
            size_t nbElements = dimParam1.elementCount();
            double* pComplex = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbElements, stringVector(), false));
            auto* outPutAsComplex = reinterpret_cast<doublecomplex*>(pComplex);
            ArrayOfView<ArrayOf> cellParam1(param1, ArrayOfViewAssumeValidated {});
            for (size_t k = 0; k < nbElements; k++) {
                ArrayOf element = cellParam1[k];
                if (element.isCharacterArray()) {
                    std::wstring str = element.asCharacterArrayWideString();
                    bool wasConverted = false;
                    outPutAsComplex[k] = stringToDoubleComplex(str, wasConverted);
                } else {
                    outPutAsComplex[k] = doublecomplex(nan(""), 0);
                }
            }
            ArrayOf output = ArrayOf(NLS_DCOMPLEX, dimOutput, pComplex, false);
            if (output.allReal()) {
                output.promoteType(NLS_DOUBLE);
            }
            retval << output;
            return retval;
        }
        Error(L"Nelson:str2double:typeNotSupported", L"Type not supported.");
    }
    // Non-text input (numbers, structures, handles, objects) is not convertible: NaN.
    retval << ArrayOf::doubleScalar(std::nan(""));
    return retval;
}
//=============================================================================
