//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_ismethodBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_ismethodBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    if (param1.handleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.asWideString();
    retval << ArrayOf::logicalScalar(param1.isHandleMethod(propertyName));
    return retval;
}
//=============================================================================
