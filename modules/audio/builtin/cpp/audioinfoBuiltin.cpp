//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioinfoBuiltin.hpp"
#include "AudioFileInfo.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// info = audiofile(filename)
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioinfoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring errorMessage;
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.getContentAsWideString();
    ArrayOf res = AudioFileInfo(filename, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    retval << res;
    return retval;
}
//=============================================================================
