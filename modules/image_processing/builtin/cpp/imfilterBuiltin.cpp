//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "imfilterBuiltin.hpp"
#include "ImageFilter.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
static std::wstring
asLowerText(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), ::towlower);
    return text;
}
//=============================================================================
ArrayOfVector
imfilterBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 5);
    nargoutcheck(nLhs, 0, 1);
    ImageFilterOptions options;
    for (size_t k = 2; k < argIn.size(); k++) {
        const ArrayOf& value = argIn[k];
        if (value.isScalarStringArray() || value.isRowVectorCharacterArray()) {
            std::wstring text = asLowerText(value);
            if (text == L"same") {
                options.shape = ImageFilterShape::Same;
            } else if (text == L"full") {
                options.shape = ImageFilterShape::Full;
            } else if (text == L"valid") {
                options.shape = ImageFilterShape::Valid;
            } else if (text == L"replicate") {
                options.padding.kind = ImageFilterPaddingKind::Replicate;
            } else if (text == L"symmetric") {
                options.padding.kind = ImageFilterPaddingKind::Symmetric;
            } else if (text == L"circular") {
                options.padding.kind = ImageFilterPaddingKind::Circular;
            } else if (text == L"corr") {
                options.convolution = false;
            } else if (text == L"conv") {
                options.convolution = true;
            } else {
                Error(L"Nelson:imfilter:unsupportedOption", L"Unsupported \"%s\" option.",
                    L"imfilter");
            }
        } else if ((value.isNumeric() || value.isLogical()) && value.isScalar()) {
            options.padding.kind = ImageFilterPaddingKind::Constant;
            options.padding.constantValue = value.asDoubleScalar();
        } else {
            Error(L"Nelson:imfilter:unsupportedOption", L"Unsupported \"%s\" option.", L"imfilter");
        }
    }
    ArrayOfVector retval;
    retval << ImageFilter(argIn[0], argIn[1], options);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
