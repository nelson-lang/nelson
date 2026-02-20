//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiodevinfoBuiltin.hpp"
#include "AudioDevInfo.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
Nelson::AudioGateway::audiodevinfoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    std::wstring errorMessage;
    ArrayOf res;
    switch (argIn.size()) {
    case 0: {
        // devinfo = audiodevinfo
        res = AudioDevInfo();
    } break;
    case 1: {
        // devinfo = audiodevinfo(io)
        // devinfo = audiodevinfo('default')
        ArrayOf param1 = argIn[0];
        if (param1.isCharacterArray()) {
            std::wstring str = param1.getContentAsWideString();
            if (str == L"default") {
                res = AudioDevInfoDefault();
            } else {
                raiseError2(_E("nelson:validators:invalidValueAtPosition"), 1);
            }
        } else {
            int io = param1.getContentAsInteger32Scalar();
            res = AudioDevInfo(io);
        }
    } break;
    case 2: {
        ArrayOf param1 = argIn[0];
        int io = param1.getContentAsInteger32Scalar();
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            // devinfo = audiodevinfo(io, name)
            std::wstring name = param2.getContentAsWideString();
            res = AudioDevInfo(io, name);
        } else {
            // devinfo = audiodevinfo(io, id)
            int id = param2.getContentAsInteger32Scalar();
            res = AudioDevInfo(io, id);
        }
    } break;
    case 3: {
        // devinfo = audiodevinfo(io, id, 'DriverVersion')
        ArrayOf param3 = argIn[2];
        std::wstring str3 = param3.getContentAsWideString();
        if (str3 != L"DriverVersion") {
            raiseError2(_E("nelson:validators:invalidValueAtPosition"), 3);
        }
        ArrayOf param1 = argIn[0];
        int io = param1.getContentAsInteger32Scalar();
        ArrayOf param2 = argIn[1];
        int id = param2.getContentAsInteger32Scalar();
        res = AudioDevInfoDriverVersion(io, id);
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
        res = AudioDevInfo(io, rate, bits, chans);
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
        res = AudioDevInfo(io, id, rate, bits, chans);
    } break;
    default: {
        raiseError2(_E("nelson:arguments:tooManyInputs"));
    } break;
    }
    retval << res;
    return retval;
}
//=============================================================================
