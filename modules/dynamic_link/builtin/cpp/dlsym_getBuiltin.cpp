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
#include "dlsym_getBuiltin.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlsym_getBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval;
    if (param1.getHandleCategory() != DLSYM_CATEGORY_STR) {
        Error(_W("dlsym handle expected."));
    }
    DynamicLinkSymbolObject* objDlsym = (DynamicLinkSymbolObject*)param1.getContentAsHandleScalar();
    ArrayOf res;
    if (!objDlsym->get(propertyName, res)) {
        Error(ERROR_WRONG_ARGUMENT_2_VALUE);
    }
    retval.push_back(res);
    return retval;
}
//=============================================================================
