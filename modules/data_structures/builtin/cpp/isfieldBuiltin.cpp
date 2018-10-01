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
#include "isfieldBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::isfieldBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isfield", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param1.isClassStruct() || param1.isHandle()) {
            retval = OverloadFunction(eval, nLhs, argIn, "isfield", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        if (param1.isStruct()) {
            if (param2.isRowVectorCharacterArray()) {
                stringVector fieldnames = param1.getFieldNames();
                std::string name = param2.getContentAsCString();
                bool res = false;
                for (size_t k = 0; k < fieldnames.size(); ++k) {
                    if (fieldnames[k].compare(name) == 0) {
                        res = true;
                    }
                }
                retval.push_back(ArrayOf::logicalConstructor(res));
            } else if (param2.isCell()) {
                stringVector fieldnames = param1.getFieldNames();
                Dimensions dims2 = param2.getDimensions();
                if (dims2.getElementCount() == 0) {
                    retval.push_back(ArrayOf::logicalConstructor(false));
                } else {
                    ArrayOf* elements = (ArrayOf*)(param2.getDataPointer());
                    logical* res
                        = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dims2.getElementCount());
                    for (size_t k = 0; k < dims2.getElementCount(); ++k) {
                        res[k] = false;
                        if (elements[k].isRowVectorCharacterArray()) {
                            std::string name = elements[k].getContentAsCString();
                            for (size_t i = 0; i < fieldnames.size(); ++i) {
                                if (fieldnames[i].compare(name) == 0) {
                                    res[k] = true;
                                }
                            }
                        }
                    }
                    retval.push_back(ArrayOf(NLS_LOGICAL, dims2, res));
                }
            } else {
                retval.push_back(ArrayOf::logicalConstructor(false));
            }
        } else {
            retval.push_back(ArrayOf::logicalConstructor(false));
        }
    }
    return retval;
}
//=============================================================================
