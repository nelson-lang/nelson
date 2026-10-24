//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "parquetinfoBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ParquetInfo.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParquetGateway::parquetinfoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring errorMessage;
    ArrayOf result = ParquetInfo(argIn[0].asWideString(), errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:parquetinfo:readFailed", errorMessage);
    }
    retval << result;
    return retval;
}
//=============================================================================
