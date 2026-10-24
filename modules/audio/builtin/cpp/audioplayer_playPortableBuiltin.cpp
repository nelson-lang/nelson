//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayer_playBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PortableAudioplayerObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOfVector
portableAudioplayerPlayBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf player = argIn[0];
    if (player.handleCategory() != NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR) {
        Error(L"Nelson:audioplayer:invalidHandle", L"audioplayer handle expected.");
    }
    auto* objPlayer = static_cast<PortableAudioplayerObject*>(player.asHandleScalar());
    int firstSample = 0;
    int lastSample = 0;
    if (argIn.size() == 2) {
        ArrayOf range = argIn[1];
        if (range.isScalar()) {
            int start = static_cast<int>(range.asDoubleScalar());
            if (start < 1) {
                Error(L"Nelson:audioplayer:invalidStart", L"start >= 1 expected.");
            }
            firstSample = start - 1;
        } else if (range.isVector() && range.isNumeric() && range.elementCount() == 2) {
            range.promoteType(NLS_DOUBLE);
            const double* ptr = static_cast<const double*>(range.dataPointer());
            int start = static_cast<int>(ptr[0]);
            int stop = static_cast<int>(ptr[1]);
            if (start < 1) {
                Error(L"Nelson:audioplayer:invalidStart", L"start >= 1 expected.");
            }
            if (stop < start) {
                Error(L"Nelson:audio:invalidRange", L"Invalid range.");
            }
            firstSample = start - 1;
            lastSample = stop - 1;
        } else {
            Error(L"Nelson:audioplayer:invalidRange", L"scalar or [start, end] vector expected.");
        }
    }
    std::wstring errorMessage;
    if (!objPlayer->play(firstSample, lastSample, errorMessage)) {
        Error(L"Nelson:audioplayer:playFailed", errorMessage);
    }
    return {};
}
//=============================================================================
ArrayOfVector
AudioGateway::audioplayer_playblockingBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return portableAudioplayerPlayBuiltin(nLhs, argIn);
}
//=============================================================================
ArrayOfVector
AudioGateway::audioplayer_playBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return portableAudioplayerPlayBuiltin(nLhs, argIn);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
