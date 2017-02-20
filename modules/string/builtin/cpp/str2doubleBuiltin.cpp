//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "str2doubleBuiltin.hpp"
#include "Error.hpp"
#include "StringToDoubleComplex.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::StringGateway::str2doubleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    retval = OverloadFunction(eval, nLhs, argIn, bSuccess);
    if (!bSuccess)
    {
        ArrayOf param1 = argIn[0];
        if (param1.isString())
        {
            std::wstring str = argIn[0].getContentsAsArrayOfCharacters();
            bool wasConverted = false;
            doublecomplex value = stringToDoubleComplex(str, wasConverted);
            ArrayOf output = ArrayOf::dcomplexConstructor(value.real(), value.imag());
            if (output.allReal())
            {
                output.promoteType(NLS_DOUBLE);
            }
            retval.push_back(output);
        }
        else
        {
            if (param1.isCell())
            {
                Dimensions dimParam1 = param1.getDimensions();
                Dimensions dimOutput(dimParam1);
                size_t nbElements = dimParam1.getElementCount();
                double *pComplex  = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbElements);
                doublecomplex* outPutAsComplex = reinterpret_cast<doublecomplex*>((double*)pComplex);
                ArrayOf *cellParam1 = (ArrayOf*)(param1.getDataPointer());
                for (size_t k = 0; k < nbElements; k++)
                {
                    ArrayOf element = cellParam1[k];
                    if (element.isString())
                    {
                        std::wstring str = element.getContentsAsArrayOfCharacters();
                        bool wasConverted = false;
                        outPutAsComplex[k] = stringToDoubleComplex(str, wasConverted);
                    }
                    else
                    {
                        outPutAsComplex[k] = doublecomplex(nan(""), 0);
                    }
                }
                ArrayOf output = ArrayOf(NLS_DCOMPLEX, dimOutput, pComplex, false);
                if (output.allReal())
                {
                    output.promoteType(NLS_DOUBLE);
                }
                retval.push_back(output);
            }
            else
            {
                Error(eval, ERROR_TYPE_NOT_SUPPORTED);
            }
        }
    }
    return retval;
}
//=============================================================================
