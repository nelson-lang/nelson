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
#include "randBuiltin.hpp"
#include "Error.hpp"
#include "Rand.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::randBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    Class cl = NLS_DOUBLE;
    if (argIn.size() == 0) {
        retval.push_back(Rand(eval, cl));
    } else {
        sizeType nRhs = argIn.size();
        bool bCheckClassName = true;
        if ((int)nRhs - 2 >= 0) {
            ArrayOf Arg = argIn[argIn.size() - 2];
            if (Arg.isRowVectorCharacterArray()) {
                std::wstring paramstr = Arg.getContentAsWideString();
                if (paramstr == L"like") {
                    ArrayOf lastArg = argIn[argIn.size() - 1];
                    switch (lastArg.getDataClass()) {
                    case NLS_DOUBLE:
                        cl = NLS_DOUBLE;
                        break;
                    case NLS_SINGLE:
                        cl = NLS_SINGLE;
                        break;
                    default:
                        Error(_W("\'single\' or \'double\' expected at last argument."));
                        break;
                    }
                    nRhs = nRhs - 2;
                    bCheckClassName = false;
                } else {
                    Error(_W("\'like\' expected at n - 2 argument."));
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
                Error(_W("\'single\' or \'double\' expected at last argument."));
            }
        }
        if (nRhs == 0) {
            retval.push_back(Rand(eval, cl));
            return retval;
        }
        Dimensions dims;
        if (nRhs == 1) {
            if (argIn[0].isNumeric() && !argIn[0].isSparse()) {
                if (argIn[0].isRowVector()) {
                    if (argIn[0].isEmpty()) {
                        Error(ERROR_WRONG_ARGUMENT_1_SIZE_ROW_VECTOR_EXPECTED);
                    }
                    if (argIn[0].getDimensions().getElementCount() < Nelson::maxDims) {
                        ArrayOf dimVector = argIn[0];
                        dimVector.promoteType(NLS_DOUBLE);
                        double* ptrValues = (double*)dimVector.getDataPointer();
                        for (sizeType k = 0; k < argIn[0].getDimensions().getElementCount(); k++) {
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
                        Error(_W("Too many dimensions! Current limit is") + L" "
                            + std::to_wstring(Nelson::maxDims) + L".");
                    }
                } else {
                    Error(ERROR_WRONG_ARGUMENT_1_SIZE_ROW_VECTOR_EXPECTED);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_NUMERIC_EXPECTED);
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
        retval.push_back(Rand(eval, dims, cl));
    }
    return retval;
}
//=============================================================================
