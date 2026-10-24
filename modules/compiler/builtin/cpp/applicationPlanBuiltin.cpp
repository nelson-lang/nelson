//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "applicationPlanBuiltin.hpp"
#include "ApplicationLinkPlan.hpp"
#include "ApplicationDistributionPlan.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <stdexcept>
//=============================================================================
namespace Nelson::CompilerGateway {
ArrayOfVector
applicationPlanBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 5);
    const auto operation = argIn[0].asWideString();
    const auto value = argIn[1].asWideString();
    ArrayOfVector result;
    try {
        if (operation == L"link") {
            std::vector<std::wstring> included;
            if (argIn.size() >= 3) {
                included = argIn[2].asWideStringVector();
            }
            bool autoDetectDataFiles = true;
            if (argIn.size() >= 4) {
                if (!argIn[3].isLogical() || !argIn[3].isScalar()) {
                    Error(L"Nelson:nelsonc:arguments:automaticDataDetectionLogicalScalar",
                        L"Automatic data detection must be a logical scalar.");
                }
                autoDetectDataFiles = argIn[3].asLogicalScalar();
            }
            result << ArrayOf::characterArray(buildApplicationLinkPlan(value, included,
                autoDetectDataFiles, argIn.size() == 5 ? argIn[4].asWideString() : L""));
        } else if (operation == L"runtime") {
            nargincheck(argIn, 3, 3);
            result << ArrayOf::characterArray(
                buildApplicationRuntimePlan(value, argIn[2].asLogicalScalar()));
        } else if (operation == L"installer-inputs") {
            nargincheck(argIn, 2, 2);
            result << ArrayOf::characterArray(collectApplicationInstallerInputs(value));
        } else if (operation == L"build-report") {
            nargincheck(argIn, 2, 2);
            result << ArrayOf::characterArray(normalizeApplicationBuildReport(value));
        } else if (operation == L"distribution") {
            nargincheck(argIn, 3, 3);
            if (!argIn[2].isLogical() || !argIn[2].isScalar()) {
                Error(L"Nelson:nelsonc:arguments:fileVerificationLogicalScalar",
                    L"File verification must be a logical scalar.");
            }
            result << ArrayOf::characterArray(
                mergeApplicationRuntimeReports(value, argIn[2].asLogicalScalar()));
        } else {
            Error(L"Nelson:nelsonc:invalidOperation:unknownApplicationPlanOperation",
                L"Unknown application plan operation.");
        }
    } catch (const std::exception& error) {
        RaiseError(error.what(), "Nelson:nelsonc:invalidPlan");
    }
    return result;
}
//=============================================================================
} // namespace Nelson::CompilerGateway
//=============================================================================
