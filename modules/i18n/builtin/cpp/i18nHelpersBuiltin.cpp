//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "i18nHelpersBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "poToJson.hpp"
#include "i18nJsonMerger.hpp"
#include "i18nJsonSort.hpp"
#include "i18n.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::I18nGateway::i18nHelpersBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    std::wstring helpersName = argIn[0].getContentAsWideString();
    if (helpersName == L"convert") {
        std::wstring poFilename = argIn[1].getContentAsWideString();
        std::wstring jsonFilename = argIn[2].getContentAsWideString();
        std::wstring errorMessage;
        if (!poToJson(poFilename, jsonFilename, errorMessage)) {
            Error(errorMessage);
        }
    } else if (helpersName == L"merge") {
        std::wstring jsonSource = argIn[1].getContentAsWideString();
        std::wstring jsonDestination = argIn[2].getContentAsWideString();
        std::wstring errorMessage;
        if (!i18nJsonMerger(jsonSource, jsonDestination, errorMessage)) {
            Error(errorMessage);
        }
    } else if (helpersName == L"sort") {
        std::wstring jsonSource = argIn[1].getContentAsWideString();
        std::wstring jsonDestination = argIn[2].getContentAsWideString();
        std::wstring errorMessage;
        if (!i18nJsonSort(jsonSource, jsonDestination, errorMessage)) {
            Error(errorMessage);
        }
    } else {
        Error(_("case not supported."));
    }
    return retval;
}
//=============================================================================
