//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_getaudiodataBuiltin.hpp"
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
Nelson::AudioGateway::audiorecorder_getaudiodataBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf handleArray = argIn[0];
    if (handleArray.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    auto* objRec = (AudiorecorderObject*)handleArray.getContentAsHandleScalar();
    if (!objRec) {
        Error(_W("Invalid audiorecorder handle."));
    }

    NelsonType destinationType = NLS_DOUBLE;
    if (argIn.size() > 1) {
        std::wstring dataType = argIn[1].getContentAsWideString();
        if (dataType == L"double") {
            destinationType = NLS_DOUBLE;
        } else if (dataType == L"single") {
            destinationType = NLS_SINGLE;
        } else if (dataType == L"int8") {
            destinationType = NLS_INT8;
        } else if (dataType == L"uint8") {
            destinationType = NLS_UINT8;
        } else if (dataType == L"int16") {
            destinationType = NLS_INT16;
        } else {
            Error(_W("Unsupported data type for audiorecorder getaudiodata."));
        }
    }

    retval << objRec->getRecordedData(destinationType);
    return retval;
}
//=============================================================================
}
//=============================================================================
