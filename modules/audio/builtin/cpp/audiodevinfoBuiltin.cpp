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
#include "audiodevinfoBuiltin.hpp"
#include "AudioDevInfo.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// devinfo = audiodevinfo
// devinfo = audiodevinfo(io)
// devinfo = audiodevinfo(io, id)
// devinfo = audiodevinfo(io, name)
// devinfo = audiodevinfo(io, id, 'DriverVersion')
// devinfo = audiodevinfo(io, rate, bits, chans)
// devinfo = audiodevinfo(io, id, rate, bits, chans)
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiodevinfoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring errorMessage;
    ArrayOf res;
    switch (argIn.size()) {
    case 0: {
        // devinfo = audiodevinfo
        res = AudioDevInfo(errorMessage);
    } break;
    case 1: {
        // devinfo = audiodevinfo(io)
        // devinfo = audiodevinfo('default')
        ArrayOf param1 = argIn[0];
        if (param1.isCharacterArray()) {
            std::wstring str = param1.getContentAsWideString();
            if (str == L"default") {
                res = AudioDevInfoDefault(errorMessage);
            } else {
                Error(_W("Wrong value for #1 argument."));
            }
        } else {
            int io = param1.getContentAsInteger32Scalar();
            res = AudioDevInfo(io, errorMessage);
        }
    } break;
    case 2: {
        ArrayOf param1 = argIn[0];
        int io = param1.getContentAsInteger32Scalar();
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            // devinfo = audiodevinfo(io, name)
            std::wstring name = param2.getContentAsWideString();
            res = AudioDevInfo(io, name, errorMessage);
        } else {
            // devinfo = audiodevinfo(io, id)
            int id = param2.getContentAsInteger32Scalar();
            res = AudioDevInfo(io, id, errorMessage);
        }
    } break;
    case 3: {
        // devinfo = audiodevinfo(io, id, 'DriverVersion')
        ArrayOf param3 = argIn[2];
        std::wstring str3 = param3.getContentAsWideString();
        if (str3 != L"DriverVersion") {
            Error(_W("Wrong value for #3 argument."));
        }
        ArrayOf param1 = argIn[0];
        int io = param1.getContentAsInteger32Scalar();
        ArrayOf param2 = argIn[1];
        int id = param2.getContentAsInteger32Scalar();
        res = AudioDevInfoDriverVersion(io, id, errorMessage);
    } break;
    case 4: {
        // devinfo = audiodevinfo(io, rate, bits, chans)
        ArrayOf param1 = argIn[0];
        int io = param1.getContentAsInteger32Scalar();
        ArrayOf param2 = argIn[1];
        int rate = param2.getContentAsInteger32Scalar();
        ArrayOf param3 = argIn[2];
        int bits = param3.getContentAsInteger32Scalar();
        ArrayOf param4 = argIn[3];
        int chans = param4.getContentAsInteger32Scalar();
        res = AudioDevInfo(io, rate, bits, chans, errorMessage);
    } break;
    case 5: {
        // devinfo = audiodevinfo(io, id, rate, bits, chans)
        ArrayOf param1 = argIn[0];
        int io = param1.getContentAsInteger32Scalar();
        ArrayOf param2 = argIn[1];
        int id = param2.getContentAsInteger32Scalar();
        ArrayOf param3 = argIn[2];
        int rate = param3.getContentAsInteger32Scalar();
        ArrayOf param4 = argIn[3];
        int bits = param4.getContentAsInteger32Scalar();
        ArrayOf param5 = argIn[4];
        int chans = param5.getContentAsInteger32Scalar();
        res = AudioDevInfo(io, id, rate, bits, chans, errorMessage);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    retval.push_back(res);
    return retval;
}
//=============================================================================
