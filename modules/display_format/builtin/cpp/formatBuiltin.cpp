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
#include "formatBuiltin.hpp"
#include "Error.hpp"
#include "NelsonConfiguration.hpp"
#include "ClassName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
DisplayFormatOptionsToArray(
    NumericFormatDisplay currentNumericFormatDisplay, LineSpacingDisplay currentLineSpacingDisplay);
//=============================================================================
static bool
setDisplayOption(const std::wstring& param1, const std::wstring& param2);
//=============================================================================
ArrayOfVector
Nelson::DisplayFormatGateway::formatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    if (nLhs == 1) {
        retval << DisplayFormatOptionsToArray(
            NelsonConfiguration::getInstance()->getNumericFormatDisplay(),
            NelsonConfiguration::getInstance()->getLineSpacingDisplay());
    }
    switch (argIn.size()) {
    case 1: {
        if ((argIn[0].isScalar() && argIn[0].isStringArray())
            || argIn[0].isRowVectorCharacterArray()) {
            std::wstring param = argIn[0].getContentAsWideString();
            boost::to_upper(param);
            if (!setDisplayOption(param, L"")) {
                Error(_W("unexpected format."));
            }
        } else if (ClassName(argIn[0]) == "DisplayFormatOptions") {
            if (argIn[0].isScalar()) {
                ArrayOf numericFormat = argIn[0].getField("NumericFormat");
                ArrayOf lineSpacing = argIn[0].getField("LineSpacing");
                if (!setDisplayOption(numericFormat.getContentAsWideString(), L"")) {
                    Error(_W("unexpected Numeric Format."));
                }
                if (!setDisplayOption(lineSpacing.getContentAsWideString(), L"")) {
                    Error(_W("unexpected Line Spacing."));
                }
            } else {
                Error(_W("Wrong size for argument #1. scalar expected"));
            }
        } else {
            Error(_W("Wrong type for argument #1. 'scalar string or row char vector expected"));
        }
    } break;
    case 2: {
        std::wstring param1;
        std::wstring param2;
        if ((argIn[0].isScalar() && argIn[0].isStringArray())
            || argIn[0].isRowVectorCharacterArray()) {
            param1 = argIn[0].getContentAsWideString();
            boost::to_upper(param1);
        } else {
            Error(_W("Wrong type for argument #1. 'scalar string or row char vector expected"));
        }
        if ((argIn[1].isScalar() && argIn[1].isStringArray())
            || argIn[1].isRowVectorCharacterArray()) {
            param2 = argIn[1].getContentAsWideString();
            boost::to_upper(param2);
        } else {
            Error(_W("Wrong type for argument #2. 'scalar string or row char vector expected"));
        }
        if (!setDisplayOption(param1, param2)) {
            Error(_W("unexpected format."));
        }
    } break;
    case 0:
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
static std::wstring
NumericFormatDisplayToString(NumericFormatDisplay currentNumericFormatDisplay)
{
    std::wstring asString;
    switch (currentNumericFormatDisplay) {
    case NLS_NUMERIC_FORMAT_SHORT: {
        asString = L"short";
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        asString = L"long";
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
        asString = L"shortE";
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
        asString = L"longE";
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
        asString = L"shortG";
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
        asString = L"longG";
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        asString = L"shortEng";
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        asString = L"longEng";
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        asString = L"+";
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        asString = L"bank";
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        asString = L"hex";
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        asString = L"rational";
    } break;
    default: {
    } break;
    }
    return asString;
}
//=============================================================================
static std::wstring
LineSpacingDisplayToString(LineSpacingDisplay currentLineSpacingDisplay)
{
    std::wstring asString;
    switch (currentLineSpacingDisplay) {
    case NLS_LINE_SPACING_COMPACT: {
        asString = L"compact";
    } break;
    case NLS_LINE_SPACING_LOOSE: {
        asString = L"loose";
    } break;
    default: {
    } break;
    }
    return asString;
}
//=============================================================================
ArrayOf
DisplayFormatOptionsToArray(
    NumericFormatDisplay currentNumericFormatDisplay, LineSpacingDisplay currentLineSpacingDisplay)
{
    Dimensions dimsRes(1, 1);
    stringVector fieldnames;
    fieldnames.reserve(2);
    fieldnames.push_back("NumericFormat");
    fieldnames.push_back("LineSpacing");
    ArrayOf* elementRes = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dimsRes.getElementCount(), fieldnames, false));
    ArrayOf res = ArrayOf(NLS_STRUCT_ARRAY, dimsRes, elementRes, false, fieldnames);

    ArrayOfVector numericFormatVector(
        ArrayOf::stringArrayConstructor(NumericFormatDisplayToString(currentNumericFormatDisplay)));
    res.setFieldAsList(fieldnames[0], numericFormatVector);
    ArrayOfVector lineSpacingVector(
        ArrayOf::stringArrayConstructor(LineSpacingDisplayToString(currentLineSpacingDisplay)));
    res.setFieldAsList(fieldnames[1], lineSpacingVector);
    res.setStructType("DisplayFormatOptions");
    return res;
}
//=============================================================================
bool
setDisplayOption(const std::wstring& param1, const std::wstring& param2)
{
    if (param1 == L"DEFAULT" && param2.empty()) {
        NelsonConfiguration::getInstance()->setLineSpacingDisplay(NLS_LINE_SPACING_LOOSE);
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_SHORT);
        return true;
    }
    if (param1 == L"LOOSE" && param2.empty()) {
        NelsonConfiguration::getInstance()->setLineSpacingDisplay(NLS_LINE_SPACING_LOOSE);
        return true;
    }
    if (param1 == L"COMPACT" && param2.empty()) {
        NelsonConfiguration::getInstance()->setLineSpacingDisplay(NLS_LINE_SPACING_COMPACT);
        return true;
    }
    if (param1 == L"SHORT" && param2.empty()) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_SHORT);
        return true;
    }
    if (param1 == L"LONG" && param2.empty()) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_LONG);
        return true;
    }
    if ((param1 == L"SHORTE" && param2.empty()) || (param1 == L"SHORT" && param2 == L"E")) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_SHORTE);
        return true;
    }
    if ((param1 == L"LONGE" && param2.empty()) || (param1 == L"LONG" && param2 == L"E")) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_LONGE);
        return true;
    }
    if ((param1 == L"SHORTG" && param2.empty()) || (param1 == L"SHORT" && param2 == L"G")) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_SHORTG);
        return true;
    }
    if ((param1 == L"LONGG" && param2.empty()) || (param1 == L"LONG" && param2 == L"G")) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_LONGG);
        return true;
    }
    if ((param1 == L"SHORTENG" && param2.empty()) || (param1 == L"SHORT" && param2 == L"ENG")) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_SHORTENG);
        return true;
    }
    if ((param1 == L"LONGENG" && param2.empty()) || (param1 == L"LONG" && param2 == L"ENG")) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_LONGENG);
        return true;
    }
    if (param1 == L"+" && param2.empty()) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_PLUS);
        return true;
    }
    if (param1 == L"BANK" && param2.empty()) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_BANK);
        return true;
    }
    if (param1 == L"HEX" && param2.empty()) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_HEX);
        return true;
    }
    if (param1 == L"RATIONAL" && param2.empty()) {
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_RATIONAL);
        return true;
    }
    return false;
}
//=============================================================================
