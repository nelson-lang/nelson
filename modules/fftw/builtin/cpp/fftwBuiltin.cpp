//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fftwBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FftHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FftwGateway::fftwBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // str = fftw('dwisdom')
    // str = fftw('swisdom')
    // method = fftw('planner')
    // previous = fftw('planner', method)
    // str = fftw('dwisdom', str)
    // previous = fftw('swisdom', str)
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    ArrayOfVector retval(nLhs);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring infoDesired = param1.getContentAsWideString();
        ArrayOf res;
        if (infoDesired == L"dwisdom") {
            res = ArrayOf::characterArrayConstructor(getDoubleWisdomInformation());
        } else if (infoDesired == L"swisdom") {
            res = ArrayOf::characterArrayConstructor(getSingleWisdomInformation());
        } else if (infoDesired == L"planner") {
            res = ArrayOf::characterArrayConstructor(getPlannerInformation());
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_VALUE);
        }
        retval << res;
    } else {
        ArrayOf param1 = argIn[0];
        std::wstring fieldname = param1.getContentAsWideString();
        ArrayOf param2 = argIn[1];
        std::wstring fieldvalue;
        bool doReset = param2.isEmpty(true);
        if (!doReset) {
            fieldvalue = param2.getContentAsWideString();
        }
        ArrayOf previousvalue;
        if (fieldname == L"dwisdom") {
            previousvalue = ArrayOf::characterArrayConstructor(getDoubleWisdomInformation());
            if (doReset) {
                resetDoubleWisdom();
            } else {
                if (!setDoubleWisdomInformation(fieldvalue)) {
                    Error(_W("Cannot apply wisdom."));
                }
            }
        } else if (fieldname == L"swisdom") {
            previousvalue = ArrayOf::characterArrayConstructor(getSingleWisdomInformation());
            if (doReset) {
                resetSingleWisdom();
            } else {
                if (!setSingleWisdomInformation(fieldvalue)) {
                    Error(_W("Cannot apply wisdom."));
                }
            }
        } else if (fieldname == L"planner") {
            previousvalue = ArrayOf::characterArrayConstructor(getPlannerInformation());
            if (doReset) {
                resetPlanner();
            } else {
                if (fieldvalue == L"estimate") {
                    setPlannerInformation(FftPlannerMethod::ESTIMATE);
                } else if (fieldvalue == L"measure") {
                    setPlannerInformation(FftPlannerMethod::MEASURE);
                } else if (fieldvalue == L"patient") {
                    setPlannerInformation(FftPlannerMethod::PATIENT);
                } else if (fieldvalue == L"exhaustive") {
                    setPlannerInformation(FftPlannerMethod::EXHAUSTIVE);
                } else if (fieldvalue == L"hybrid") {
                    setPlannerInformation(FftPlannerMethod::HYBRID);
                } else {
                    Error(ERROR_WRONG_ARGUMENT_1_VALUE);
                }
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_VALUE);
        }
        retval << previousvalue;
    }
    return retval;
}
//=============================================================================
