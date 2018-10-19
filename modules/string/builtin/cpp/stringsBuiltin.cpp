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
#include "stringsBuiltin.hpp"
#include "Error.hpp"
#include "StringFormat.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::stringsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        retval.push_back(ArrayOf::stringArrayConstructor(std::string("")));
    }
    if (argIn.size() == 1) {
        if (argIn[0].getDataClass() == NLS_DOUBLE) {
            if (argIn[0].isVector() || argIn[0].isScalar()) {
                if (argIn[0].isScalar()) {
                    ArrayOf arg = argIn[0];
                    double dindex = arg.getContentAsDoubleScalar();
                    if (!std::isfinite(dindex)) {
                        Error(ERROR_WRONG_ARGUMENT_1_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dindex < 0) {
                        dindex = 0;
                    }
                    indexType index = (indexType)dindex;
                    if ((double)index != dindex) {
                        Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    Dimensions dims(index, index);
                    ArrayOf* elements = new ArrayOf[index * index];
                    for (indexType k = 0; k < index * index; k++) {
                        elements[k] = ArrayOf::characterArrayConstructor("");
                    }
                    ArrayOf c = ArrayOf(NLS_STRING_ARRAY, dims, elements);
                    retval.push_back(c);
                } else {
                    ArrayOf arg = argIn[0];
                    Dimensions dims(arg.getLength());
                    double* dindex = (double*)arg.getDataPointer();
                    for (indexType k = 0; k < (indexType)arg.getLength(); k++) {
                        double _dIndex = dindex[k];
                        if (!std::isfinite(_dIndex)) {
                            Error(ERROR_WRONG_ARGUMENT_1_FINITE_VECTOR_INTEGER_VALUE_EXPECTED);
                        }
                        if (_dIndex < 0) {
                            _dIndex = 0;
                        }
                        indexType index = (indexType)_dIndex;
                        if ((double)index != _dIndex) {
                            Error(ERROR_WRONG_ARGUMENT_1_FINITE_VECTOR_INTEGER_VALUE_EXPECTED);
                        }
                        dims.setDimensionLength(k, index);
                    }
                    dims.simplify();
                    ArrayOf* elements = new ArrayOf[dims.getElementCount()];
                    for (indexType k = 0; k < (indexType)dims.getElementCount(); k++) {
                        elements[k] = ArrayOf::characterArrayConstructor("");
                    }
                    ArrayOf c = ArrayOf(NLS_STRING_ARRAY, dims, elements);
                    retval.push_back(c);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_OR_ROW_VECTOR_EXPECTED);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        }
    } else {
        Dimensions dims(argIn.size());
        for (sizeType k = 0; k < (sizeType)argIn.size(); k++) {
            if (argIn[k].getDataClass() == NLS_DOUBLE) {
                if (argIn[k].isScalar()) {
                    ArrayOf arg = argIn[k];
                    double dindex = arg.getContentAsDoubleScalar();
                    if (!std::isfinite(dindex)) {
                        Error(StringFormat(
                            ERROR_WRONG_ARGUMENT_X_FINITE_SCALAR_INTEGER_VALUE_EXPECTED.c_str(),
                            k + 1));
                    }
                    if (dindex < 0) {
                        dindex = 0;
                    }
                    indexType index = (indexType)dindex;
                    if ((double)index != dindex) {
                        Error(StringFormat(
                            ERROR_WRONG_ARGUMENT_X_FINITE_SCALAR_INTEGER_VALUE_EXPECTED.c_str(),
                            k + 1));
                    }
                    dims.setDimensionLength(k, index);
                } else {
                    Error(StringFormat(ERROR_WRONG_ARGUMENT_X_SIZE_SCALAR_EXPECTED.c_str(), k + 1));
                }
            } else {
                Error(StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_DOUBLE_EXPECTED.c_str(), k + 1));
            }
        }
        dims.simplify();
        ArrayOf* elements = new ArrayOf[dims.getElementCount()];
        for (indexType k = 0; k < (indexType)dims.getElementCount(); k++) {
            elements[k] = ArrayOf::characterArrayConstructor("");
        }
        ArrayOf c = ArrayOf(NLS_STRING_ARRAY, dims, elements);
        retval.push_back(c);
    }
    return retval;
}
//=============================================================================
