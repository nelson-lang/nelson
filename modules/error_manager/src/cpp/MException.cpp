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
#include <boost/algorithm/string.hpp>
#include <algorithm>
#include "MException.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline bool
isAlphaNum(wchar_t c)
{
    return (iswalpha(c) || iswdigit(c));
}
//=============================================================================
bool
isValidMExceptionIdentifier(std::wstring identifier)
{
    wstringVector splittedComponents;
    boost::split(splittedComponents, identifier, boost::is_any_of(L":"));
    if (splittedComponents.size() < 2) {
        return false;
    }
    for (std::wstring component : splittedComponents) {
        if (component.empty()) {
            return false;
        }
        if (!iswalpha(component[0])) {
            return false;
        }
        if (component.size() > 1) {
            component.erase(0, 1);
            if (find_if(component.begin(), component.end(), isAlphaNum) == component.end()) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
}
//=============================================================================
