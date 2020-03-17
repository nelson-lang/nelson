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
#include "fullpathBuiltin.hpp"
#include "Error.hpp"
#include "NormalizePath.hpp"
#include "IsCellOfStrings.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::fullpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (IsCellOfString(param1) || param1.isStringArray()) {
        Dimensions dims = param1.getDimensions();
        wstringVector paths = param1.getContentAsWideStringVector();
        wstringVector normalizedPaths;
        normalizedPaths.reserve(dims.getElementCount());
        for (std::wstring s : paths) {
            normalizedPaths.push_back(NormalizePath(s));
        }
        if (param1.isStringArray()) {
            retval.push_back(ArrayOf::stringArrayConstructor(normalizedPaths, dims));
        } else {
            retval.push_back(ToCellStringAsColumn(normalizedPaths));
        }
    } else {
        std::wstring path = argIn[0].getContentAsWideString();
        retval.push_back(ArrayOf::characterArrayConstructor(NormalizePath(path)));
    }
    return retval;
}
//=============================================================================
