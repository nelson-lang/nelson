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
#include "NelsonConfiguration.hpp"
#include "ArrayOf.hpp"
//=============================================================================
#pragma once
//=============================================================================
namespace Nelson {
//=============================================================================
class FormatDisplayInformation
{
public:
    //=============================================================================
    FormatDisplayInformation()
        : isComplex(false)
        , floatAsInteger(false)
        , numericFormatDisplay(NLS_NUMERIC_FORMAT_SHORT)
        , lineSpacingDisplay(NLS_LINE_SPACING_LOOSE)
        , formatReal(L"%*.*f")
        , widthReal(9)
        , decimalsReal(4)
        , formatImag(L"%*.*f")
        , widthImag(9)
        , decimalsImag(4)
        , scaleFactor(1.0)
        , trim(false){};
    //=============================================================================
    bool isComplex;
    bool floatAsInteger;

    double scaleFactor;
    bool trim;

    NumericFormatDisplay numericFormatDisplay;
    LineSpacingDisplay lineSpacingDisplay;

    std::wstring formatReal;
    size_t widthReal;
    size_t decimalsReal;

    std::wstring formatImag;
    size_t widthImag;
    size_t decimalsImag;
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
