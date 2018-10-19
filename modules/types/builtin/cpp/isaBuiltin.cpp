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
#include "isaBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isaBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (!param2.isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    std::wstring classnameExpected = param2.getContentAsWideString();
    if (classnameExpected == L"numeric") {
        retval.push_back(ArrayOf::logicalConstructor(param1.isNumeric()));
    } else if (classnameExpected == L"float") {
        bool bRes = (param1.getDataClass() == NLS_DOUBLE || param1.getDataClass() == NLS_DCOMPLEX
            || param1.getDataClass() == NLS_SINGLE || param1.getDataClass() == NLS_SCOMPLEX);
        retval.push_back(ArrayOf::logicalConstructor(bRes));
    } else if (classnameExpected == L"integer") {
        bool bRes = (param1.getDataClass() == NLS_UINT8 || param1.getDataClass() == NLS_INT8
            || param1.getDataClass() == NLS_UINT16 || param1.getDataClass() == NLS_INT16
            || param1.getDataClass() == NLS_UINT32 || param1.getDataClass() == NLS_INT32
            || param1.getDataClass() == NLS_UINT64 || param1.getDataClass() == NLS_INT64);
        retval.push_back(ArrayOf::logicalConstructor(bRes));
    } else {
        bool res = false;
        std::wstring currentClassName;
        ClassName(param1, currentClassName);
        if (currentClassName == utf8_to_wstring(NLS_HANDLE_STR)) {
            if (classnameExpected == utf8_to_wstring(NLS_HANDLE_STR)) {
                res = true;
            } else {
                res = (param1.getHandleCategory() == classnameExpected);
            }
        } else {
            res = (currentClassName == classnameExpected);
        }
        retval.push_back(ArrayOf::logicalConstructor(res));
    }
    return retval;
}
//=============================================================================
