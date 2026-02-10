//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_isrecordingBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_isrecordingBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf handleArray = argIn[0];
    if (handleArray.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        raiseError(L"Nelson:audio:ERROR_AUDIORECORDER_HANDLE_EXPECTED",
            ERROR_AUDIORECORDER_HANDLE_EXPECTED);
    }
    auto* objRec = (AudiorecorderObject*)handleArray.getContentAsHandleScalar();
    if (!objRec) {
        raiseError(
            L"Nelson:audio:ERROR_INVALID_AUDIORECORDER_HANDLE", ERROR_INVALID_AUDIORECORDER_HANDLE);
    }
    retval << ArrayOf::logicalConstructor(objRec->getRunning());
    return retval;
}
//=============================================================================
}
//=============================================================================
