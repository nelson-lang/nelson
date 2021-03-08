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
#include <string>
#include <algorithm>
#include <cstdlib>
#include <unordered_map>
#include "AsciiToDouble.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::unordered_map<std::string, double> map;
//=============================================================================
double
asciiToDouble(const std::string& str)
{
    std::unordered_map<std::string, double>::const_iterator found = map.find(str);
    if (found != map.end()) {
        return found->second;
    }
    std::string s(str);
    std::replace(s.begin(), s.end(), 'D', 'E');
    std::replace(s.begin(), s.end(), 'd', 'e');
    double value = atof(s.c_str());
    map.emplace(str, value);
    return value;
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
