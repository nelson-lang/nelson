//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "parquetwriteBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ParquetBuiltinHelpers.hpp"
#include "ParquetWrite.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParquetGateway::parquetwriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 2, 3);
    std::wstring filename = argIn[0].asWideString();
    ParquetWriteOptions options
        = argIn.size() > 2 ? getWriteOptions(argIn[2]) : ParquetWriteOptions {};
    std::wstring errorMessage;
    ParquetWrite(filename, argIn[1], options, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:parquetwrite:writeFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
