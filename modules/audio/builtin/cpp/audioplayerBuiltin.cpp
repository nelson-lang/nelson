//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayerBuiltin.hpp"
#include "AudioHelpers.hpp"
#include "AudioplayerObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// playerObj = audioplayer(Y, Fs)
// playerObj = audioplayer(Y, Fs, nBits)
// playerObj = audioplayer(Y, Fs, nBits, ID)
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    AudioplayerObject* objPlayer = nullptr;
    std::wstring errorMessage = _W("Cannot create audioplayer handle.");
    bool res = false;
    bool haveEventLoop = NelsonConfiguration::getInstance()->haveEventsLoop();
    switch (argIn.size()) {
    case 2: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        int sampleRate = param2.getContentAsInteger32Scalar();
        objPlayer = new AudioplayerObject(haveEventLoop);
        res = objPlayer->setSamples(argIn[0], sampleRate, errorMessage);
    } break;
    case 3: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        int sampleRate = param2.getContentAsInteger32Scalar();
        ArrayOf param3 = argIn[2];
        int bitsPerSample = param3.getContentAsInteger32Scalar();
        objPlayer = new AudioplayerObject(haveEventLoop);
        res = objPlayer->setSamples(argIn[0], sampleRate, bitsPerSample, errorMessage);
    } break;
    case 4: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        int sampleRate = param2.getContentAsInteger32Scalar();
        ArrayOf param3 = argIn[2];
        int bitsPerSample = param3.getContentAsInteger32Scalar();
        ArrayOf param4 = argIn[3];
        int deviceID = param4.getContentAsInteger32Scalar();
        objPlayer = new AudioplayerObject(haveEventLoop);
        res = objPlayer->setSamples(argIn[0], sampleRate, bitsPerSample, deviceID, errorMessage);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    if (!res) {
        delete objPlayer;
        Error(errorMessage);
    } else {
        retval << ArrayOf::handleConstructor(objPlayer);
    }
    return retval;
}
//=============================================================================
