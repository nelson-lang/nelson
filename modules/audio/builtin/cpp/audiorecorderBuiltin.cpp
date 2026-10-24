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
#include "ErrorID.hpp"
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
    nargincheck(argIn, 0, 4);
    switch (argIn.size()) {
    case 0: {
        objRec = new AudiorecorderObject(haveEventLoop);
    } break;
    case 3: {
        int sampleRate = argIn[0].asInt32Scalar();
        int bits = argIn[1].asInt32Scalar();
        int channels = argIn[2].asInt32Scalar();
        objRec = new AudiorecorderObject(haveEventLoop);
        objRec->setConfig(sampleRate, bits, channels, -1);
    } break;
    case 4: {
        int sampleRate = argIn[0].asInt32Scalar();
        int bits = argIn[1].asInt32Scalar();
        int channels = argIn[2].asInt32Scalar();
        int deviceID = argIn[3].asInt32Scalar();
        objRec = new AudiorecorderObject(haveEventLoop);
        objRec->setConfig(sampleRate, bits, channels, deviceID);
    } break;
    default: {
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    } break;
    }
    if (!objRec) {
        Error(L"Nelson:audiorecorder:createHandleFailed", L"Cannot create audiorecorder handle.");
    } else {
        retval << ArrayOf::handle(objRec);
    }
    return retval;
}
//=============================================================================
