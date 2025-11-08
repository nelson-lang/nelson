//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_playBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "audioplayerBuiltin.hpp"
#include "audioplayer_playBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_playBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    auto* objRec = (AudiorecorderObject*)param1.getContentAsHandleScalar();
    // get recorded data and feed to audioplayer playBuiltin via existing play function
    ArrayOf recorded = objRec->getRecordedData(NLS_DOUBLE);
    if (recorded.isEmpty()) {
        Error(_W("No recorded data."));
    }
    // call existing playBuiltin overload by forwarding
    ArrayOfVector args;
    args.push_back(recorded);
    // optional second arg: sample rate
    args.push_back(ArrayOf::doubleConstructor(objRec->getSampleRate()));
    // Use existing global playBuiltin (not implemented here) — instead forward to playBuiltin in
    // gateway We call the generic playBuiltin entrypoint via function pointer not available here;
    // instead rely on user to call play(recorded, Fs) For convenience, try to call
    // audioplayerBuiltin to create player then play
    ArrayOfVector createArgs;
    createArgs.push_back(recorded);
    createArgs.push_back(ArrayOf::doubleConstructor(objRec->getSampleRate()));
    ArrayOfVector out = Nelson::AudioGateway::audioplayerBuiltin(1, createArgs);
    if (!out.empty()) {
        // out[0] is player handle, call play
        ArrayOf playerHandle = out[0];
        ArrayOfVector playArgs;
        playArgs.push_back(playerHandle);
        Nelson::AudioGateway::audioplayer_playBuiltin(eval, 0, playArgs);
    }
    return retval;
}
//=============================================================================
