//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_setBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.asWideString();
    ArrayOf param3 = argIn[2];
    ArrayOfVector retval;
    if (param1.handleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    auto* objRec = (AudiorecorderObject*)param1.asHandleScalar();
    std::wstring errorMessage;
    if (!objRec->set(propertyName, param3, errorMessage)) {
        Error(L"Nelson:audiorecorder:setFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
