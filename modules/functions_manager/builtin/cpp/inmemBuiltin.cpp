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
#include "inmemBuiltin.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "PathFuncManager.hpp"
#include "MacroFunctionDef.hpp"
#include "MexFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::inmemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 0, 1);

    bool withCompleteNames = false;
    if (argIn.size() == 1) {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"-completenames") {
            withCompleteNames = true;
        }
    }

    wstringVector mFunctions
        = FunctionsInMemory::getInstance()->getMacroInMemory(withCompleteNames);
    retval << ToCellStringAsColumn(mFunctions);
    if (nLhs > 1) {
        wstringVector mexFunctions
            = FunctionsInMemory::getInstance()->getMexInMemory(withCompleteNames);
        retval << ToCellStringAsColumn(mexFunctions);
    }
    if (nLhs > 2) {
        retval << ToCellStringAsColumn(stringVector());
    }
    return retval;
}
//=============================================================================
