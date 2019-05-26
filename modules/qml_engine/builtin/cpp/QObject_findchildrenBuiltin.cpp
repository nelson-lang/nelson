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
#include "QObject_findchildrenBuiltin.hpp"
#include "Error.hpp"
#include "findchildrenQObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_findchildrenBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (!(argIn.size() == 2 || argIn.size() == 3)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring fieldname = param2.getContentAsWideString();
    bool bRecursively = false;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        logical l = param3.getContentAsLogicalScalar();
        bRecursively = (l == 1);
    }
    retval.push_back(findchildrenQObject(param1, fieldname, bRecursively));
    return retval;
}
//=============================================================================
