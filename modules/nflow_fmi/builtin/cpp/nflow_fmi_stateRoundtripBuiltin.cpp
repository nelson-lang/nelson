//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_fmi_stateRoundtripBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FmuComponent.hpp"
#include "SimEngineTypes.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include <cmath>
//=============================================================================
// __nflow_fmi_state_roundtrip__(fmuPath [, dt]): exercises the FMU state
// snapshot/rollback primitive (canGetAndSetFMUState). Steps once, saves, steps
// and records the outputs, rolls back, steps again with the same input and
// records the outputs; the two must match if save/restore is correct. Returns a
// struct with fields .supported (logical) and .match (logical). Test-only.
//=============================================================================
namespace Nelson::NFlowFmiGateway {
//=============================================================================
ArrayOfVector
nflow_fmi_stateRoundtripBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    const std::string path = wstring_to_utf8(argIn[0].asWideString());
    double dt = 0.1;
    if (argIn.size() > 1) {
        dt = argIn[1].asDoubleScalar();
    }

    Nelson::NFlow::SimDiagnostic diag;
    std::unique_ptr<Nelson::NFlow::IFmuComponent> comp
        = Nelson::NFlow::makeFmuComponent(path, diag);
    if (!comp) {
        Error(
            L"Nelson:nflow_fmi:loadFailed", L"Cannot load FMU: %s", utf8_to_wstring(diag.message));
    }
    if (!comp->initialize(0.0, dt * 4.0, {}, {}, diag)) {
        Error(L"Nelson:nflow_fmi:initializationFailed", L"FMU initialization failed: %s",
            utf8_to_wstring(diag.message));
    }

    const bool supported = comp->canSaveState();
    bool match = false;
    if (supported) {
        const int nOut = comp->totalOutputElements();
        std::vector<double> a(std::max(1, nOut), 0.0), b(std::max(1, nOut), 0.0);
        if (!comp->doStep(0.0, dt, diag)) {
            Error(L"Nelson:nflow_fmi:stepFailed", L"FMU step failed: %s",
                utf8_to_wstring(diag.message));
        }
        if (!comp->saveState(diag)) {
            Error(L"Nelson:nflow_fmi:stateSnapshotFailed", L"FMU state snapshot failed: %s",
                utf8_to_wstring(diag.message));
        }
        if (!comp->doStep(dt, dt, diag)) {
            Error(L"Nelson:nflow_fmi:stepFailed", L"FMU step failed: %s",
                utf8_to_wstring(diag.message));
        }
        if (!comp->getOutputs(a.data(), nOut, diag)) {
            Error(L"Nelson:nflow_fmi:outputReadFailed", L"FMU output read failed: %s",
                utf8_to_wstring(diag.message));
        }
        if (!comp->restoreState(diag)) {
            Error(L"Nelson:nflow_fmi:stateRestoreFailed", L"FMU state restore failed: %s",
                utf8_to_wstring(diag.message));
        }
        if (!comp->doStep(dt, dt, diag)) {
            Error(L"Nelson:nflow_fmi:stepFailed", L"FMU step failed: %s",
                utf8_to_wstring(diag.message));
        }
        if (!comp->getOutputs(b.data(), nOut, diag)) {
            Error(L"Nelson:nflow_fmi:outputReadFailed", L"FMU output read failed: %s",
                utf8_to_wstring(diag.message));
        }
        match = true;
        for (int i = 0; i < nOut; ++i) {
            if (std::abs(a[i] - b[i]) > 1e-12) {
                match = false;
                break;
            }
        }
    }

    stringVector fieldnames { "supported", "match" };
    ArrayOfVector fieldvalues;
    fieldvalues << ArrayOf::logicalScalar(supported);
    fieldvalues << ArrayOf::logicalScalar(match);
    ArrayOfVector retval;
    retval << ArrayOf::structScalar(fieldnames, fieldvalues);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowFmiGateway
//=============================================================================
