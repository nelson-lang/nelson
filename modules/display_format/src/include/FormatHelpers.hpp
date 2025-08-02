//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsDisplay_format_exports.h"
#include "ArrayOfFormatInfo.hpp"
#include "NelsonConfiguration.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatScalarNumber(double val, bool asSingle, const FormatDisplayInformation& formatInfo);
//=============================================================================
std::wstring
formatScalarComplexNumber(
    double realPart, double imagPart, bool asSingle, const FormatDisplayInformation& formatInfo);
//=============================================================================
std::wstring
formatElement(double val, const FormatDisplayInformation& formatInfo);
//=============================================================================
std::wstring
formatElementComplex(double realPart, double imagPart, const FormatDisplayInformation& formatInfo);
//=============================================================================
std::wstring
formatElement(single val, const FormatDisplayInformation& formatInfo);
//=============================================================================
std::wstring
formatElementComplex(single realPart, single ImagPart, const FormatDisplayInformation& formatInfo);
//=============================================================================
std::wstring
formatScaleFactor(const FormatDisplayInformation& formatInfo);
//=============================================================================
std::wstring
centerText(const std::wstring& text, size_t width);
//=============================================================================
indexType
getNominalWidth(const FormatDisplayInformation& formatInfo);
}
//=============================================================================
