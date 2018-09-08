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
#include "IsWarningStruct.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsWarningStruct(ArrayOf arg, wstringVector& identifiers, wstringVector& states)
{
    identifiers.clear();
    states.clear();

    if (!arg.isStruct()) {
        return false;
    }
    stringVector fs = arg.getFieldNames();
    if (fs.size() != 2) {
        return false;
    }
    if (fs[0] != "identifier") {
        return false;
    }
    if (fs[1] != "state") {
        return false;
    }
    ArrayOfVector idArray = arg.getFieldAsList("identifier");
    ArrayOfVector stateArray = arg.getFieldAsList("state");

    for (size_t k = 0; k < idArray.size(); k++) {
        try {
            identifiers.push_back(idArray[k].getContentAsWideString());
        } catch (const Exception&) {
            return false;
        }
    }
    for (size_t k = 0; k < stateArray.size(); k++) {
        try {
            states.push_back(stateArray[k].getContentAsWideString());
        } catch (const Exception&) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
