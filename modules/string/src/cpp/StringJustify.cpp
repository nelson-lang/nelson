//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "StringJustify.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline std::wstring_view
stringTrimView(const std::wstring& str)
{
    auto start = str.find_first_not_of(L' ');
    if (start == std::wstring::npos)
        return std::wstring_view();
    auto end = str.find_last_not_of(L' ');
    return std::wstring_view(str.data() + start, end - start + 1);
}
//=============================================================================
static inline std::wstring
stringJustifyLeft(const std::wstring& str)
{
    return fmt::format(L"{:<{}}", stringTrimView(str), str.length());
}
//=============================================================================
static inline std::wstring
stringJustifyCenter(const std::wstring& str)
{
    return fmt::format(L"{:^{}}", stringTrimView(str), str.length());
}
//=============================================================================
static inline std::wstring
stringJustifyRight(const std::wstring& str)
{
    return fmt::format(L"{:>{}}", stringTrimView(str), str.length());
}
//=============================================================================
ArrayOf
StringJustify(const ArrayOf& stringArrayOf, STRINGJUSTIFY style)
{
    switch (stringArrayOf.getDataClass()) {
    case NLS_CHAR: {
        std::wstring str = stringArrayOf.getContentAsWideString();
        switch (style) {
        case STRINGJUSTIFY::NLS_JUSTIFY_CENTER: {
            return ArrayOf::characterArrayConstructor(stringJustifyCenter(str));
        } break;
        case STRINGJUSTIFY::NLS_JUSTIFY_RIGHT: {
            return ArrayOf::characterArrayConstructor(stringJustifyRight(str));
        } break;
        default:
        case STRINGJUSTIFY::NLS_JUSTIFY_LEFT: {
            return ArrayOf::characterArrayConstructor(stringJustifyLeft(str));
        } break;
        }
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY: {
        ArrayOf* ptr = (ArrayOf*)stringArrayOf.getDataPointer();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            stringArrayOf.getDataClass(), stringArrayOf.getElementCount());
        ArrayOf cell
            = ArrayOf(stringArrayOf.getDataClass(), stringArrayOf.getDimensions(), elements);
        OMP_PARALLEL_FOR_LOOP(stringArrayOf.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)stringArrayOf.getElementCount(); ++k) {
            if (ptr[k].isRowVectorCharacterArray()) {
                elements[k] = StringJustify(ptr[k], style);
            } else {
                elements[k] = ArrayOf::doubleConstructor(std::nan("NaN"));
            }
        }
        return cell;
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return {};
}
//=============================================================================
};
//=============================================================================
