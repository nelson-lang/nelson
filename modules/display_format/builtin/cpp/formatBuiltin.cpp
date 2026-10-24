//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "formatBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
#include "ClassdefParser.hpp"
#include "ClassName.hpp"
#include "StringHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define DISPLAY_FORMAT_OPTIONS_CLASS "nelson.display.DisplayFormatOptions"
#define DISPLAY_FORMAT_OPTIONS_NUMERIC_FORMAT_FIELD "NumericFormatValue"
#define DISPLAY_FORMAT_OPTIONS_LINE_SPACING_FIELD "LineSpacingValue"
#define DISPLAY_FORMAT_OPTIONS_TRUNCATE_MATRICES_FIELD "TruncateMatricesValue"
//=============================================================================
static ArrayOf
DisplayFormatOptionsToArray(NumericFormatDisplay currentNumericFormatDisplay,
    LineSpacingDisplay currentLineSpacingDisplay, bool currentTruncateMatricesDisplay);
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
            NelsonConfiguration::getInstance()->getLineSpacingDisplay(),
            NelsonConfiguration::getInstance()->getTruncateMatricesDisplay());
    }
    switch (argIn.size()) {
    case 1: {
        if ((argIn[0].isScalar() && argIn[0].isStringArray())
            || argIn[0].isRowVectorCharacterArray()) {
            std::wstring param = argIn[0].asWideString();
            StringHelpers::to_upper(param);
            if (!setDisplayOption(param, L"")) {
                Error(L"Nelson:format:unexpectedFormat", L"unexpected format.");
            }
        } else if (ClassName(argIn[0]) == DISPLAY_FORMAT_OPTIONS_CLASS) {
            if (argIn[0].isScalar()) {
                ArrayOf numericFormat = argIn[0].field(DISPLAY_FORMAT_OPTIONS_NUMERIC_FORMAT_FIELD);
                ArrayOf lineSpacing = argIn[0].field(DISPLAY_FORMAT_OPTIONS_LINE_SPACING_FIELD);
                ArrayOf truncateMatrices
                    = argIn[0].field(DISPLAY_FORMAT_OPTIONS_TRUNCATE_MATRICES_FIELD);
                std::wstring numericFormatAsString = numericFormat.asWideString();
                StringHelpers::to_upper(numericFormatAsString);
                if (!setDisplayOption(numericFormatAsString, L"")) {
                    Error(L"Nelson:format:unexpectedNumericFormat", L"unexpected Numeric Format.");
                }
                std::wstring lineSpacingAsString = lineSpacing.asWideString();
                StringHelpers::to_upper(lineSpacingAsString);
                if (!setDisplayOption(lineSpacingAsString, L"")) {
                    Error(L"Nelson:format:unexpectedLineSpacing", L"unexpected Line Spacing.");
                }
                std::wstring truncateMatricesAsString = truncateMatrices.asWideString();
                StringHelpers::to_upper(truncateMatricesAsString);
                if (!setDisplayOption(L"TRUNCATEMATRICES", truncateMatricesAsString)) {
                    Error(L"Nelson:format:unexpectedTruncateMatrices",
                        L"unexpected Truncate Matrices.");
                }
            } else {
                Error(L"Nelson:format:scalarExpected",
                    L"Wrong size for argument #1. scalar expected");
            }
        } else {
            Error(L"Nelson:format:invalidType",
                L"Wrong type for argument #%d. 'scalar string or row char vector expected", 1);
        }
    } break;
    case 2: {
        std::wstring param1;
        std::wstring param2;
        if ((argIn[0].isScalar() && argIn[0].isStringArray())
            || argIn[0].isRowVectorCharacterArray()) {
            param1 = argIn[0].asWideString();
            StringHelpers::to_upper(param1);
        } else {
            Error(L"Nelson:format:invalidType",
                L"Wrong type for argument #%d. 'scalar string or row char vector expected", 1);
        }
        if ((argIn[1].isScalar() && argIn[1].isStringArray())
            || argIn[1].isRowVectorCharacterArray()) {
            param2 = argIn[1].asWideString();
            StringHelpers::to_upper(param2);
        } else {
            Error(L"Nelson:format:invalidType",
                L"Wrong type for argument #%d. 'scalar string or row char vector expected", 2);
        }
        if (!setDisplayOption(param1, param2)) {
            Error(L"Nelson:format:unexpectedFormat", L"unexpected format.");
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
static std::wstring
TruncateMatricesDisplayToString(bool currentTruncateMatricesDisplay)
{
    return currentTruncateMatricesDisplay ? L"on" : L"off";
}
//=============================================================================
ArrayOf
DisplayFormatOptionsToArray(NumericFormatDisplay currentNumericFormatDisplay,
    LineSpacingDisplay currentLineSpacingDisplay, bool currentTruncateMatricesDisplay)
{
    ClassdefLayoutPtr layout = classdefLayout(DISPLAY_FORMAT_OPTIONS_CLASS);
    if (!layout) {
        Error(L"Nelson:format:classNotFound", L"DisplayFormatOptions class not found.");
    }
    Dimensions dimsRes(1, 1);
    ArrayOf* elementRes = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
        NLS_CLASSDEF_ARRAY, dimsRes.elementCount(), layout->storagePropertyNames, false));
    ArrayOf res
        = ArrayOf(NLS_CLASSDEF_ARRAY, dimsRes, elementRes, false, layout->storagePropertyNames);
    res.setClassType(DISPLAY_FORMAT_OPTIONS_CLASS);
    res.setClassdefLayout(layout);

    ArrayOfVector numericFormatVector(
        ArrayOf::stringArray(NumericFormatDisplayToString(currentNumericFormatDisplay)));
    res.setFieldAsList(DISPLAY_FORMAT_OPTIONS_NUMERIC_FORMAT_FIELD, numericFormatVector);
    ArrayOfVector lineSpacingVector(
        ArrayOf::stringArray(LineSpacingDisplayToString(currentLineSpacingDisplay)));
    res.setFieldAsList(DISPLAY_FORMAT_OPTIONS_LINE_SPACING_FIELD, lineSpacingVector);
    ArrayOfVector truncateMatricesVector(
        ArrayOf::stringArray(TruncateMatricesDisplayToString(currentTruncateMatricesDisplay)));
    res.setFieldAsList(DISPLAY_FORMAT_OPTIONS_TRUNCATE_MATRICES_FIELD, truncateMatricesVector);
    return res;
}
//=============================================================================
bool
setDisplayOption(const std::wstring& param1, const std::wstring& param2)
{
    if (param1 == L"DEFAULT" && param2.empty()) {
        NelsonConfiguration::getInstance()->setLineSpacingDisplay(NLS_LINE_SPACING_LOOSE);
        NelsonConfiguration::getInstance()->setNumericFormatDisplay(NLS_NUMERIC_FORMAT_SHORT);
        NelsonConfiguration::getInstance()->setTruncateMatricesDisplay(true);
        return true;
    }
    if (param1 == L"TRUNCATEMATRICES" && param2 == L"ON") {
        NelsonConfiguration::getInstance()->setTruncateMatricesDisplay(true);
        return true;
    }
    if (param1 == L"TRUNCATEMATRICES" && param2 == L"OFF") {
        NelsonConfiguration::getInstance()->setTruncateMatricesDisplay(false);
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
