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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PortableAudioplayerObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 4);
    int sampleRate = argIn[1].asInt32Scalar();
    int bitsPerSample = 16;
    if (argIn.size() >= 3) {
        bitsPerSample = argIn[2].asInt32Scalar();
    }
    auto* objPlayer = new PortableAudioplayerObject();
    std::wstring errorMessage;
    if (!objPlayer->setSamples(argIn[0], sampleRate, bitsPerSample, errorMessage)) {
        delete objPlayer;
        Error(L"Nelson:audioplayer:creationFailed", errorMessage);
    }
    ArrayOfVector retval;
    retval << ArrayOf::handle(objPlayer);
    return retval;
}
//=============================================================================
