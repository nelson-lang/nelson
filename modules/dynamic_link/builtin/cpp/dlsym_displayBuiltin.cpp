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
#include "dlsym_displayBuiltin.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlsym_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    if (eval == nullptr) {
        return retval;
    }
    Interface* io = eval->getInterface();
    if (io == nullptr) {
        return retval;
    }
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        DisplayVariableHeader(io, param1, name);
        Dimensions dimsParam1 = param1.getDimensions();
        io->outputMessage(L"[dlsym] - size: ");
        dimsParam1.printMe(io);
        io->outputMessage("\n");
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != DLSYM_CATEGORY_STR) {
                Error(_W("dlsym handle expected."));
            }
            auto* dlsymObj = (DynamicLinkSymbolObject*)param1.getContentAsHandleScalar();
            dlsymObj->disp(eval);
        }
        DisplayVariableFooter(io, param1, name);
    } else {
        Error(_W("dlsym handle expected."));
    }
    return retval;
}
//=============================================================================
