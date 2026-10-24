//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xlsfinfoBuiltin.hpp"
#include "ExcelWorkbookIO.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::xlsfinfoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 3);
    nargincheck(argIn, 1, 1);

    std::wstring filename = argIn[0].asWideString();
    if (!isExcelWorkbookExtension(filename)) {
        Error(L"Nelson:spreadsheet:onlyXlsxSupported",
            L"Only .xlsx spreadsheet files are supported by this backend.");
    }

    stringVector sheetNames;
    std::wstring format;
    std::wstring errorMessage;
    bool status = ExcelWorkbookInfo(filename, sheetNames, format, errorMessage);
    if (!status) {
        retval << ArrayOf::characterArray("");
        if (nLhs > 1) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(stringVector {});
        }
        if (nLhs > 2) {
            retval << ArrayOf::characterArray("");
        }
        return retval;
    }

    retval << ArrayOf::characterArray("Open XML Spreadsheet");
    if (nLhs > 1) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(sheetNames);
    }
    if (nLhs > 2) {
        retval << ArrayOf::characterArray(format);
    }
    return retval;
}
//=============================================================================
