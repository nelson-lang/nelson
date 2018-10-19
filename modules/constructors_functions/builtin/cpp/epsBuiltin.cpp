//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "epsBuiltin.hpp"
#include "Epsilon.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::epsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (argIn.size() == 0) {
        retval.push_back(ArrayOf::doubleConstructor(Epsilon((double)1.0)));
    } else {
        if (argIn[0].getDataClass() == NLS_DOUBLE || argIn[0].getDataClass() == NLS_DCOMPLEX) {
            if (!argIn[0].isScalar()) {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
            }
            double* pV = (double*)argIn[0].getDataPointer();
            double dV = pV[0];
            retval.push_back(ArrayOf::doubleConstructor(Epsilon((double)dV)));
        } else if (argIn[0].getDataClass() == NLS_SINGLE
            || argIn[0].getDataClass() == NLS_SCOMPLEX) {
            if (!argIn[0].isScalar()) {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
            }
            single* pV = (single*)argIn[0].getDataPointer();
            single dV = pV[0];
            retval.push_back(ArrayOf::singleConstructor(Epsilon((single)dV)));
        } else if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring arg = argIn[0].getContentAsWideString();
            if (arg.compare(L"single") == 0) {
                retval.push_back(ArrayOf::singleConstructor(Epsilon((single)1.0)));
            } else if (arg.compare(L"double") == 0) {
                retval.push_back(ArrayOf::doubleConstructor(Epsilon((double)1.0)));
            } else {
                Error(_W("Type \'double\' or \'single\' expected."));
            }
        } else {
            Error(_W("Type \'double\' or \'single\' expected."));
        }
    }
    return retval;
}
//=============================================================================
