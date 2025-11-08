//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_recordBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <thread>
#include <chrono>
#include <cmath>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_recordBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    double durationSeconds = std::nan("");
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (!param2.isScalar() || !param2.isNumeric()) {
            Error(_W("duration in seconds expected."));
        }
        double duration = param2.getContentAsDoubleScalar();
    }

    if (durationSeconds > 0.0) {
        Error(_W("duration > 0 expected."));
    }
    auto* objRec = (AudiorecorderObject*)param1.getContentAsHandleScalar();
    if (objRec) {
        objRec->record(durationSeconds);
    }

    return retval;
}
//=============================================================================
