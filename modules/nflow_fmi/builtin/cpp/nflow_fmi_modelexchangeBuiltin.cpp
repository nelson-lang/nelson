//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_fmi_modelexchangeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FmiPackage.hpp"
#include "FmiInstance.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include <cmath>
#include <vector>
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow::Fmi;
//=============================================================================
// __nflow_fmi_modelexchange__(unzippedFmuDir, tStop [, dt]) -> struct
//
// Imports an FMI 3.0 Model Exchange FMU and integrates it with NFlow's own
// fixed-step RK4 solver, with step-boundary state-event handling. Records every
// Float64 output at each communication point. Returns:
//   .time        (N x 1)
//   .outputNames (1 x nOut)
//   .outputs     (N x nOut)
//=============================================================================
ArrayOfVector
Nelson::NFlowFmiGateway::nflow_fmi_modelexchangeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);

    if (!fmiSupportCompiledIn()) {
        Error("Nelson:nflow_fmi:notCompiled", "FMI support is not compiled in this build.");
    }

    const std::string dir = wstring_to_utf8(argIn[0].asWideString());
    const double tStop = argIn[1].asDoubleScalar();
    double dt = (argIn.size() >= 3) ? argIn[2].asDoubleScalar() : 0.0;
    // Optional relative tolerance passed to the FMU at init (arg 4). > 0 marks it as
    // defined; <= 0 keeps the FMU default.
    const double relTol = (argIn.size() >= 4) ? argIn[3].asDoubleScalar() : 0.0;
    if (!(tStop > 0.0)) {
        Error("Nelson:nflow_fmi:badInputs:tStop", "tStop must be a positive scalar.");
    }
    if (dt <= 0.0) {
        dt = tStop / 1000.0; // ME needs finer steps than CS (no internal solver)
    }
    if (dt > tStop) {
        dt = tStop;
    }

    std::string err;
    std::unique_ptr<FmiPackage> pkg = FmiPackage::loadUnzipped("nelson_me", dir, err);
    if (!pkg) {
        RaiseError(err, "Nelson:nflow_fmi:loadFailed");
    }
    const FmiModelInfo& mi = pkg->info();
    if (!mi.supportsModelExchange) {
        Error("Nelson:nflow_fmi:unsupportedFmu", "FMU does not support %s.", "Model Exchange");
    }

    std::vector<uint32_t> outVr;
    std::vector<std::string> outNames;
    for (const FmiVariable& v : mi.variables) {
        if (v.causality == FmiCausality::Output && v.dataType == FmiDataType::Float64) {
            outVr.push_back(v.valueReference);
            outNames.push_back(v.name);
        }
    }
    const size_t nOut = outVr.size();

    std::unique_ptr<FmiInstance> inst = pkg->instantiateModelExchange("nelson_me", false, err);
    if (!inst) {
        RaiseError(err, "Nelson:nflow_fmi:initializationFailed");
    }
    if (!inst->enterInitializationMode(relTol > 0.0, relTol, 0.0, true, tStop, err)) {
        RaiseError(err, "Nelson:nflow_fmi:initializationFailed");
    }
    if (!inst->exitInitializationMode(err)) {
        RaiseError(err, "Nelson:nflow_fmi:initializationFailed");
    }
    if (!inst->enterContinuousTimeMode(err)) {
        RaiseError(err, "Nelson:nflow_fmi:initializationFailed");
    }

    const size_t nx = inst->numberOfContinuousStates();
    const size_t nz = inst->numberOfEventIndicators();
    std::vector<double> x;
    if (!inst->getContinuousStates(x, nx, err)) {
        RaiseError(err, "Nelson:nflow_fmi:stepFailed");
    }
    std::vector<double> prevG;
    inst->getEventIndicators(prevG, nz, err);

    auto derivatives = [&](double tt, const std::vector<double>& xx, std::vector<double>& dx) {
        std::string e;
        inst->setTime(tt, e);
        inst->setContinuousStates(xx, e);
        inst->getDerivatives(dx, nx, e);
    };

    const int nSteps = static_cast<int>(std::floor((tStop + 0.5 * dt) / dt));
    const size_t nRows = static_cast<size_t>(nSteps) + 1;

    std::vector<double> timeCol;
    timeCol.reserve(nRows);
    std::vector<double> outFlat;
    outFlat.assign(nRows * nOut, 0.0);

    std::vector<double> k1(nx), k2(nx), k3(nx), k4(nx), xt(nx), values;
    double t = 0.0;
    for (size_t row = 0; row < nRows; ++row) {
        timeCol.push_back(t);
        inst->setTime(t, err);
        inst->setContinuousStates(x, err);
        if (nOut > 0) {
            if (!inst->getFloat64(outVr, values, err)) {
                RaiseError(err, "Nelson:nflow_fmi:outputReadFailed");
            }
            for (size_t j = 0; j < nOut; ++j) {
                outFlat[row + j * nRows] = values[j];
            }
        }
        if (row + 1 >= nRows) {
            break;
        }

        if (nx > 0) {
            derivatives(t, x, k1);
            for (size_t i = 0; i < nx; ++i) {
                xt[i] = x[i] + 0.5 * dt * k1[i];
            }
            derivatives(t + 0.5 * dt, xt, k2);
            for (size_t i = 0; i < nx; ++i) {
                xt[i] = x[i] + 0.5 * dt * k2[i];
            }
            derivatives(t + 0.5 * dt, xt, k3);
            for (size_t i = 0; i < nx; ++i) {
                xt[i] = x[i] + dt * k3[i];
            }
            derivatives(t + dt, xt, k4);
            for (size_t i = 0; i < nx; ++i) {
                x[i] += (dt / 6.0) * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
            }
        }
        t += dt;

        inst->setTime(t, err);
        inst->setContinuousStates(x, err);
        bool enterEvent = false;
        bool terminateRequested = false;
        if (!inst->completedIntegratorStep(enterEvent, terminateRequested, err)) {
            RaiseError(err, "Nelson:nflow_fmi:stepFailed");
        }
        std::vector<double> g;
        inst->getEventIndicators(g, nz, err);
        bool crossing = false;
        for (size_t i = 0; i < nz && i < prevG.size(); ++i) {
            if ((prevG[i] < 0.0) != (g[i] < 0.0)) {
                crossing = true;
                break;
            }
        }
        if (enterEvent || crossing) {
            if (!inst->enterEventMode(err)) {
                RaiseError(err, "Nelson:nflow_fmi:initializationFailed");
            }
            bool valuesChanged = false;
            bool term = false;
            if (!inst->updateDiscreteStates(valuesChanged, term, err)) {
                RaiseError(err, "Nelson:nflow_fmi:stepFailed");
            }
            if (valuesChanged) {
                inst->getContinuousStates(x, nx, err);
            }
            if (!inst->enterContinuousTimeMode(err)) {
                RaiseError(err, "Nelson:nflow_fmi:initializationFailed");
            }
            inst->getEventIndicators(g, nz, err);
            if (term) {
                terminateRequested = true;
            }
        }
        prevG = g;
        if (terminateRequested) {
            timeCol.resize(row + 2);
            timeCol.back() = t;
            break;
        }
    }

    std::string termError;
    inst->terminate(termError);

    const size_t actualRows = timeCol.size();
    double* timePtr = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, actualRows, stringVector(), false));
    for (size_t i = 0; i < actualRows; ++i) {
        timePtr[i] = timeCol[i];
    }
    ArrayOf timeArray(NLS_DOUBLE, Dimensions(actualRows, 1), timePtr);

    double* outPtr = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, actualRows * nOut, stringVector(), false));
    for (size_t j = 0; j < nOut; ++j) {
        for (size_t i = 0; i < actualRows; ++i) {
            outPtr[i + j * actualRows] = outFlat[i + j * nRows];
        }
    }
    ArrayOf outArray(NLS_DOUBLE, Dimensions(actualRows, nOut), outPtr);

    ArrayOf* nameCells = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nOut, stringVector(), false));
    for (size_t j = 0; j < nOut; ++j) {
        nameCells[j] = ArrayOf::characterArray(outNames[j]);
    }
    ArrayOf namesArray(NLS_CELL_ARRAY, Dimensions(nOut > 0 ? 1 : 0, nOut), nameCells);

    stringVector fieldnames = { "time", "outputNames", "outputs" };
    ArrayOfVector fieldvalues;
    fieldvalues << timeArray;
    fieldvalues << namesArray;
    fieldvalues << outArray;

    ArrayOfVector retval;
    retval << ArrayOf::structScalar(fieldnames, fieldvalues);
    return retval;
}
//=============================================================================
