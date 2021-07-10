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

    wstringVector mFunctions;
    boost::unordered_map<std::string, FuncPtr> macroInMemory
        = FunctionsInMemory::getInstance()->getMacroInMemory();
    for (auto it = macroInMemory.begin(); it != macroInMemory.end(); ++it) {
        MacroFunctionDef* fptr = (MacroFunctionDef*)it->second;
        if (withCompleteNames) {
            mFunctions.push_back(fptr->getFilename());
        } else {
            mFunctions.push_back(utf8_to_wstring(fptr->getName()));
        }
    }
    retval << ToCellStringAsColumn(mFunctions);
    if (nLhs > 1) {
        wstringVector mexFunctions;
        boost::unordered_map<std::string, FuncPtr> mexInMemory
            = FunctionsInMemory::getInstance()->getMexInMemory();
        for (auto it = mexInMemory.begin(); it != mexInMemory.end(); ++it) {
            MexFunctionDef* fptr = (MexFunctionDef*)it->second;
            if (withCompleteNames) {
                mexFunctions.push_back(fptr->getFilename());
            } else {
                mexFunctions.push_back(utf8_to_wstring(fptr->getName()));
            }
        }
        retval << ToCellStringAsColumn(mexFunctions);
    }
    if (nLhs > 2) {
        retval << ToCellStringAsColumn(stringVector());
    }
    return retval;
}
//=============================================================================
