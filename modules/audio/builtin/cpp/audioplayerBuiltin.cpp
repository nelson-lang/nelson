//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayerBuiltin.hpp"
#include "AudioHelpers.hpp"
#include "AudioplayerObject.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// playerObj = audioplayer(Y, Fs)
// playerObj = audioplayer(Y, Fs, nBits)
// playerObj = audioplayer(Y, Fs, nBits, ID)
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    AudioplayerObject* objPlayer = nullptr;
    std::wstring errorMessage = _W("Cannot create audioplayer handle.");
    bool res = false;
    switch (argIn.size()) {
    case 2: {
        ArrayOf param2 = argIn[1];
        int sampleRate = param2.getContentAsInteger32Scalar();
        objPlayer = new AudioplayerObject();
        if (objPlayer) {
            res = objPlayer->setSamples(argIn[0], sampleRate, errorMessage);
        }
    } break;
    case 3: {
        ArrayOf param2 = argIn[1];
        int sampleRate = param2.getContentAsInteger32Scalar();
        ArrayOf param3 = argIn[2];
        int bitsPerSample = param3.getContentAsInteger32Scalar();
        objPlayer = new AudioplayerObject();
        if (objPlayer) {
            res = objPlayer->setSamples(argIn[0], sampleRate, bitsPerSample, errorMessage);
        }
    } break;
    case 4: {
        ArrayOf param2 = argIn[1];
        int sampleRate = param2.getContentAsInteger32Scalar();
        ArrayOf param3 = argIn[2];
        int bitsPerSample = param3.getContentAsInteger32Scalar();
        ArrayOf param4 = argIn[3];
        int deviceID = param4.getContentAsInteger32Scalar();
        objPlayer = new AudioplayerObject();
        if (objPlayer) {
            res = objPlayer->setSamples(
                argIn[0], sampleRate, bitsPerSample, deviceID, errorMessage);
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    if (!res) {
        if (objPlayer) {
            delete objPlayer;
        }
        Error(errorMessage);
    }
    retval.push_back(ArrayOf::handleConstructor(objPlayer));
    return retval;
}
//=============================================================================
