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
#include "FullFile.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#if _MSC_VER
#define FILE_SEPW L"\\"
#else
#define FILE_SEPW L"/"
#endif
//=============================================================================
static std::wstring
FullFilename(const std::wstring& path)
{
    std::wstring result = path;
    bool haveSeparatorAtTheEnd
        = boost::algorithm::ends_with(path, L"\\") || boost::algorithm::ends_with(path, L"/");
#if _MSC_VER
    bool isUNC = boost::algorithm::starts_with(path, L"\\\\");
    boost::replace_all(result, L"/", FILE_SEPW);
    boost::replace_all(result, L"\\.\\", L"");
    while (boost::algorithm::contains(result, L"\\\\")) {
        boost::replace_all(result, L"\\\\", FILE_SEPW);
    }
    if (isUNC) {
        boost::replace_first(result, FILE_SEPW, L"\\\\");
    }
#else
    boost::replace_all(result, L"\\", FILE_SEPW);
    boost::replace_all(result, L"/./", L"");
    while (boost::algorithm::contains(result, L"//")) {
        boost::replace_all(result, L"//", FILE_SEPW);
    }

#endif
    if (haveSeparatorAtTheEnd && !boost::algorithm::ends_with(result, FILE_SEPW)) {
        result = result + FILE_SEPW;
    }
    return result;
}
//=============================================================================
std::wstring
FullFile(const wstringVector& parts)
{
    std::wstring result;
    indexType nbParts = parts.size();
    for (indexType k = 0; k < nbParts; ++k) {
        if (k == 0) {
            result = parts[k];
        } else {
            result = result + FILE_SEPW + parts[k];
        }
    }
    return FullFilename(result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
