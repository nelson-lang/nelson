//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
        , trim(false) {};
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
