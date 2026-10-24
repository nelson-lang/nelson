//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "parquetreadBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ParquetBuiltinHelpers.hpp"
#include "ParquetRead.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParquetGateway::parquetreadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 4);
    std::wstring filename = argIn[0].asWideString();
    stringVector selectedNames = argIn.size() > 1 ? getStringList(argIn[1]) : stringVector {};
    std::vector<int> rowGroups = argIn.size() > 2 ? getRowGroups(argIn[2]) : std::vector<int> {};
    ArrayOf rowFilterExpression = argIn.size() > 3 ? argIn[3] : ArrayOf {};
    std::wstring errorMessage;
    ArrayOf result
        = ParquetRead(filename, selectedNames, rowGroups, rowFilterExpression, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:parquetread:readFailed", errorMessage);
    }
    retval << result;
    return retval;
}
//=============================================================================
