//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorderBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorderBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);

    bool haveEventLoop = NelsonConfiguration::getInstance()->haveEventsLoop();

    AudiorecorderObject* objRec = nullptr;
    switch (argIn.size()) {
    case 0: {
        objRec = new AudiorecorderObject(haveEventLoop);
    } break;
    case 3: {
        int sampleRate = argIn[0].getContentAsInteger32Scalar();
        int bits = argIn[1].getContentAsInteger32Scalar();
        int channels = argIn[2].getContentAsInteger32Scalar();
        objRec = new AudiorecorderObject(haveEventLoop);
        objRec->setConfig(sampleRate, bits, channels, -1);
    } break;
    case 4: {
        int sampleRate = argIn[0].getContentAsInteger32Scalar();
        int bits = argIn[1].getContentAsInteger32Scalar();
        int channels = argIn[2].getContentAsInteger32Scalar();
        int deviceID = argIn[3].getContentAsInteger32Scalar();
        objRec = new AudiorecorderObject(haveEventLoop);
        objRec->setConfig(sampleRate, bits, channels, deviceID);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    if (!objRec) {
        Error(_W("Cannot create audiorecorder handle."));
    } else {
        retval << ArrayOf::handleConstructor(objRec);
    }
    return retval;
}
//=============================================================================
