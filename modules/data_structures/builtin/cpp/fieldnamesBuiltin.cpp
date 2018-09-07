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
#include "fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "ToCellString.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::fieldnamesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "fieldnames", bSuccess);
    }
    if (!bSuccess) {
        if (argIn.size() != 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        ArrayOf arg1 = argIn[0];
        if (arg1.isClassStruct() || arg1.isHandle()) {
            retval = OverloadFunction(eval, nLhs, argIn, "fieldnames", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(utf8_to_wstring(arg1.getStructType()) + L"_fieldnames " + _W("not defined."));
        } else {
            if (arg1.isStruct()) {
                if (arg1.isEmpty()) {
                    Dimensions dim(0, 1);
                    ArrayOf res = ArrayOf::emptyConstructor(dim);
                    res.promoteType(NLS_CELL_ARRAY);
                    retval.push_back(res);
                } else {
                    stringVector fieldnames = arg1.getFieldNames();
                    retval.push_back(ToCellStringAsColumn(fieldnames));
                }
            } else {
                retval = OverloadFunction(eval, nLhs, argIn, "fieldnames", bSuccess);
                if (bSuccess) {
                    return retval;
                }
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
            }
        }
    }
    return retval;
}
//=============================================================================
