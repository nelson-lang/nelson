//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "str2doubleBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "StringToDoubleComplex.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::str2doubleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "str2double", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf param1 = argIn[0];
        if (param1.isCharacterArray() || param1.isStringArray() || param1.isCell()) {
            if (param1.isCharacterArray()) {
                std::wstring str = argIn[0].getContentAsArrayOfCharacters();
                bool wasConverted = false;
                doublecomplex value = stringToDoubleComplex(str, wasConverted);
                ArrayOf output = ArrayOf::dcomplexConstructor(value.real(), value.imag());
                if (output.allReal()) {
                    output.promoteType(NLS_DOUBLE);
                }
                retval << output;
                return retval;
            }
            if (param1.isStringArray() || param1.isCell()) {
                Dimensions dimParam1 = param1.getDimensions();
                Dimensions dimOutput(dimParam1);
                size_t nbElements = dimParam1.getElementCount();
                double* pComplex = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbElements, stringVector(), false));
                auto* outPutAsComplex = reinterpret_cast<doublecomplex*>(pComplex);
                auto* cellParam1 = (ArrayOf*)(param1.getDataPointer());
                for (size_t k = 0; k < nbElements; k++) {
                    ArrayOf element = cellParam1[k];
                    if (element.isCharacterArray()) {
                        std::wstring str = element.getContentAsArrayOfCharacters();
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
            retval = OverloadFunction(eval, nLhs, argIn, "str2double", bSuccess);
            if (!bSuccess) {
                Error(ERROR_TYPE_NOT_SUPPORTED);
            }
            return retval;
        }
    }
    return OverloadFunction(eval, nLhs, argIn, "str2double");
}
//=============================================================================
