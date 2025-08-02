//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlmwriteBuiltin.hpp"
#include "DelimitedFileWriter.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// dlmwrite(filename, M)  rhs == 2
// dlmwrite(filename, M, delimiter, r, c, eol, precision) rhs == 7
// dlmwrite(filename, M, delimiter, r, c, eol) rhs == 6
// dlmwrite(filename, M, delimiter, r, c) rhs == 5
// dlmwrite(filename, M, delimiter) rhs == 3
// dlmwrite(filename, M, '-append', delimiter, r, c, eol, precision) rhs == 8
// dlmwrite(filename, M, '-append', delimiter, r, c, eol) rhs == 7
// dlmwrite(filename, M, '-append', delimiter, r, c) rhs == 6
// dlmwrite(filename, M, '-append', delimiter) rhs == 4
// dlmwrite(filename, M, '-append') rhs == 3
//=============================================================================
static std::wstring DEFAULT_DELIMITER = L",";
static std::wstring DEFAULT_FORMAT = L"%.5g";
static indexType DEFAULT_ROWS_OFFSET = 0;
static indexType DEFAULT_COLS_OFFSET = 0;
static bool DEFAULT_APPEND_MODE = false;
#ifdef _MSC_VER
static bool DEFAULT_EOF_ISPC = true;
#else
static bool DEFAULT_EOF_ISPC = false;
#endif
//=============================================================================
using dlmOptions = struct
{
    std::wstring delimiter;
    std::wstring fmt;
    int64 rowsOffset;
    int64 colsOffset;
    bool isPcEOL;
    bool isAppend;
};
//=============================================================================
static dlmOptions
dlmwriteBuiltinTwoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dlmwrite(filename, M)
    dlmOptions res;
    res.delimiter = DEFAULT_DELIMITER;
    res.fmt = DEFAULT_FORMAT;
    res.rowsOffset = DEFAULT_ROWS_OFFSET;
    res.colsOffset = DEFAULT_COLS_OFFSET;
    res.isPcEOL = DEFAULT_EOF_ISPC;
    res.isAppend = DEFAULT_APPEND_MODE;
    return res;
}
//=============================================================================
static dlmOptions
dlmwriteBuiltinThreeRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dlmwrite(filename, M, delimiter)
    // dlmwrite(filename, M, '-append')
    dlmOptions res;
    res.delimiter = DEFAULT_DELIMITER;
    res.fmt = DEFAULT_FORMAT;
    res.rowsOffset = DEFAULT_ROWS_OFFSET;
    res.colsOffset = DEFAULT_COLS_OFFSET;
    res.isPcEOL = DEFAULT_EOF_ISPC;
    res.isAppend = DEFAULT_APPEND_MODE;
    ArrayOf param3 = argIn[2];
    std::wstring paramStr = param3.getContentAsWideString();
    if (paramStr == L"-append") {
        res.isAppend = true;
    } else {
        StringHelpers::replace_all(paramStr, L"\\t", L"\t");
        StringHelpers::replace_all(paramStr, L"\\n", L"\n");
        StringHelpers::replace_all(paramStr, L"\\r", L"\r");
        res.delimiter = paramStr;
    }
    return res;
}
//=============================================================================
static dlmOptions
dlmwriteBuiltinFourRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dlmwrite(filename, M, '-append', delimiter)
    dlmOptions res;
    res.delimiter = DEFAULT_DELIMITER;
    res.fmt = DEFAULT_FORMAT;
    res.rowsOffset = DEFAULT_ROWS_OFFSET;
    res.colsOffset = DEFAULT_COLS_OFFSET;
    res.isPcEOL = DEFAULT_EOF_ISPC;
    res.isAppend = DEFAULT_APPEND_MODE;
    ArrayOf param3 = argIn[2];
    std::wstring paramStr = param3.getContentAsWideString();
    if (paramStr == L"-append") {
        res.isAppend = true;
    } else {
        Error(_W("'-append' expected."));
    }
    ArrayOf param4 = argIn[3];
    paramStr = param4.getContentAsWideString();
    StringHelpers::replace_all(paramStr, L"\\t", L"\t");
    StringHelpers::replace_all(paramStr, L"\\n", L"\n");
    StringHelpers::replace_all(paramStr, L"\\r", L"\r");
    res.delimiter = paramStr;
    return res;
}
//=============================================================================
static dlmOptions
dlmwriteBuiltinFiveRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dlmwrite(filename, M, delimiter, r, c)
    dlmOptions res;
    res.delimiter = DEFAULT_DELIMITER;
    res.fmt = DEFAULT_FORMAT;
    res.rowsOffset = DEFAULT_ROWS_OFFSET;
    res.colsOffset = DEFAULT_COLS_OFFSET;
    res.isPcEOL = DEFAULT_EOF_ISPC;
    res.isAppend = DEFAULT_APPEND_MODE;
    ArrayOf param3 = argIn[2];
    std::wstring paramStr = param3.getContentAsWideString();
    if (paramStr == L"-append") {
        Error(_W("a valid delimiter expected."));
    }
    StringHelpers::replace_all(paramStr, L"\\t", L"\t");
    StringHelpers::replace_all(paramStr, L"\\n", L"\n");
    StringHelpers::replace_all(paramStr, L"\\r", L"\r");
    res.delimiter = paramStr;
    ArrayOf param4 = argIn[3];
    ArrayOf param5 = argIn[4];
    res.rowsOffset = param4.getContentAsInteger64Scalar();
    res.colsOffset = param5.getContentAsInteger64Scalar();
    return res;
}
//=============================================================================
static dlmOptions
dlmwriteBuiltinSixRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    dlmOptions res;
    res.delimiter = DEFAULT_DELIMITER;
    res.fmt = DEFAULT_FORMAT;
    res.rowsOffset = DEFAULT_ROWS_OFFSET;
    res.colsOffset = DEFAULT_COLS_OFFSET;
    res.isPcEOL = DEFAULT_EOF_ISPC;
    res.isAppend = DEFAULT_APPEND_MODE;
    ArrayOf param3 = argIn[2];
    std::wstring paramStr = param3.getContentAsWideString();
    if (paramStr == L"-append") {
        // dlmwrite(filename, M, '-append', delimiter, r, c) rhs == 6
        res.isAppend = true;
        ArrayOf param4 = argIn[3];
        paramStr = param4.getContentAsWideString();
        StringHelpers::replace_all(paramStr, L"\\t", L"\t");
        StringHelpers::replace_all(paramStr, L"\\n", L"\n");
        StringHelpers::replace_all(paramStr, L"\\r", L"\r");
        res.delimiter = paramStr;
        ArrayOf param5 = argIn[4];
        ArrayOf param6 = argIn[5];
        res.rowsOffset = param5.getContentAsInteger64Scalar();
        res.colsOffset = param6.getContentAsInteger64Scalar();
    } else {
        // dlmwrite(filename, M, delimiter, r, c, eol) rhs == 6
        StringHelpers::replace_all(paramStr, L"\\t", L"\t");
        StringHelpers::replace_all(paramStr, L"\\n", L"\n");
        StringHelpers::replace_all(paramStr, L"\\r", L"\r");
        res.delimiter = paramStr;
        ArrayOf param4 = argIn[3];
        ArrayOf param5 = argIn[4];
        res.rowsOffset = param4.getContentAsInteger64Scalar();
        res.colsOffset = param5.getContentAsInteger64Scalar();
        ArrayOf param6 = argIn[5];
        std::wstring eolStr = param6.getContentAsWideString();
        if (eolStr == L"pc" || eolStr == L"unix") {
            res.isPcEOL = (eolStr == L"pc");
        } else {
            Error(_W("'pc' or 'unix' expected."));
        }
    }
    return res;
}
//=============================================================================
static dlmOptions
dlmwriteBuiltinSevenRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    dlmOptions res = dlmwriteBuiltinSixRhs(eval, nLhs, argIn);
    ArrayOf param3 = argIn[2];
    std::wstring paramStr = param3.getContentAsWideString();
    ArrayOf param7 = argIn[6];
    if (paramStr == L"-append") {
        // dlmwrite(filename, M, '-append', delimiter, r, c, eol) rhs == 7
        std::wstring eolStr = param7.getContentAsWideString();
        if (eolStr == L"pc" || eolStr == L"unix") {
            res.isPcEOL = (eolStr == L"pc");
        } else {
            Error(_W("'pc' or 'unix' expected."));
        }
    } else {
        // dlmwrite(filename, M, delimiter, r, c, eol, precision) rhs == 7
        if (param7.isCharacterArray()) {
            res.fmt = param7.getContentAsWideString();
        } else {
            indexType precision = param7.getContentAsScalarIndex(true);
            res.fmt = L"%." + std::to_wstring(precision) + L"g";
        }
    }
    return res;
}
//=============================================================================
static dlmOptions
dlmwriteBuiltinEightRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dlmwrite(filename, M, '-append', delimiter, r, c, eol, precision) rhs == 8
    dlmOptions res;
    res.delimiter = DEFAULT_DELIMITER;
    res.fmt = DEFAULT_FORMAT;
    res.rowsOffset = DEFAULT_ROWS_OFFSET;
    res.colsOffset = DEFAULT_COLS_OFFSET;
    res.isPcEOL = DEFAULT_EOF_ISPC;
    res.isAppend = DEFAULT_APPEND_MODE;
    ArrayOf param3 = argIn[2];
    std::wstring paramStr = param3.getContentAsWideString();
    if (paramStr != L"-append") {
        Error(_W("'-append' expected."));
    }
    res.isAppend = true;
    ArrayOf param4 = argIn[3];
    paramStr = param4.getContentAsWideString();
    StringHelpers::replace_all(paramStr, L"\\t", L"\t");
    StringHelpers::replace_all(paramStr, L"\\n", L"\n");
    StringHelpers::replace_all(paramStr, L"\\r", L"\r");
    res.delimiter = paramStr;
    ArrayOf param5 = argIn[4];
    ArrayOf param6 = argIn[5];
    res.rowsOffset = param5.getContentAsInteger64Scalar();
    res.colsOffset = param6.getContentAsInteger64Scalar();
    ArrayOf param7 = argIn[6];
    std::wstring eolStr = param7.getContentAsWideString();
    if (eolStr == L"pc" || eolStr == L"unix") {
        res.isPcEOL = (eolStr == L"pc");
    } else {
        Error(_W("'pc' or 'unix' expected."));
    }
    ArrayOf param8 = argIn[7];
    if (param8.isCharacterArray()) {
        res.fmt = param8.getContentAsWideString();
    } else {
        indexType precision = param8.getContentAsScalarIndex(true);
        res.fmt = L"%." + std::to_wstring(precision) + L"g";
    }
    return res;
}
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::dlmwriteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (param2.isCell()) {
        Context* context = eval->getContext();
        FunctionDef* funcDef = nullptr;
        std::string cell2matName = "cell2mat";
        if (context->lookupFunction(cell2matName, funcDef)) {
            if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                ArrayOfVector argInCopy;
                argInCopy.push_back(param2);
                try {
                    ArrayOfVector resVect = funcDef->evaluateFunction(eval, argInCopy, 1);
                    if (resVect.size() != 1) {
                        Error(_W("cell2mat returns more than one output argument."));
                    }
                    param2 = resVect[0];
                } catch (const Exception&) {
                    throw;
                }
            }
        } else {
            Error("cell2mat function not found.");
        }
    }
    std::wstring filename = param1.getContentAsWideString();
    dlmOptions opts;
    switch (argIn.size()) {
    case 2: {
        opts = dlmwriteBuiltinTwoRhs(eval, nLhs, argIn);
    } break;
    case 3: {
        opts = dlmwriteBuiltinThreeRhs(eval, nLhs, argIn);
    } break;
    case 4: {
        opts = dlmwriteBuiltinFourRhs(eval, nLhs, argIn);
    } break;
    case 5: {
        opts = dlmwriteBuiltinFiveRhs(eval, nLhs, argIn);
    } break;
    case 6: {
        opts = dlmwriteBuiltinSixRhs(eval, nLhs, argIn);
    } break;
    case 7: {
        opts = dlmwriteBuiltinSevenRhs(eval, nLhs, argIn);
    } break;
    case 8: {
        opts = dlmwriteBuiltinEightRhs(eval, nLhs, argIn);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    delimitedFileWriter(param2, filename, opts.isAppend, opts.delimiter, opts.rowsOffset,
        opts.colsOffset, opts.fmt, opts.isPcEOL);
    return retval;
}
//=============================================================================
