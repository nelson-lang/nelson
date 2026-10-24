//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_fmi_cosimBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FmiPackage.hpp"
#include "FmiInstance.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include <cmath>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow::Fmi;
//=============================================================================
// __nflow_fmi_cosim__(unzippedFmuDir, tStop [, dt]) -> struct
//
// Runs a fixed-step Co-Simulation of an already-extracted FMI 3.0 FMU with no
// external inputs (parameters/inputs keep their start values) and records every
// Float64 output at each communication point. Returns:
//   .time        (N x 1)      communication points
//   .outputNames (1 x nOut)   cell of output variable names
//   .outputs     (N x nOut)   output values over time
//=============================================================================
ArrayOfVector
Nelson::NFlowFmiGateway::nflow_fmi_cosimBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 6);
    nargoutcheck(nLhs, 0, 1);

    if (!fmiSupportCompiledIn()) {
        Error("Nelson:nflow_fmi:notCompiled", "FMI support is not compiled in this build.");
    }

    const std::string dir = wstring_to_utf8(argIn[0].asWideString());
    const double tStop = argIn[1].asDoubleScalar();
    double dt = (argIn.size() >= 3) ? argIn[2].asDoubleScalar() : 0.0;
    // Optional relative tolerance (arg 6, after the constant-input pair). > 0 marks it
    // as defined at enterInitializationMode; <= 0 keeps the FMU default.
    const double relTol = (argIn.size() >= 6) ? argIn[5].asDoubleScalar() : 0.0;

    if (!(tStop > 0.0)) {
        Error("Nelson:nflow_fmi:badInputs:tStop", "tStop must be a positive scalar.");
    }
    if (dt <= 0.0) {
        dt = tStop / 100.0; // sensible default: 100 steps
    }
    if (dt > tStop) {
        dt = tStop;
    }

    std::string errorMessage;
    std::unique_ptr<FmiPackage> pkg = FmiPackage::loadUnzipped("nelson_cosim", dir, errorMessage);
    if (!pkg) {
        RaiseError(errorMessage, "Nelson:nflow_fmi:loadFailed");
    }

    const FmiModelInfo& mi = pkg->info();
    if (!mi.supportsCoSimulation) {
        Error("Nelson:nflow_fmi:unsupportedFmu", "FMU does not support %s.", "Co-Simulation");
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

    // Optional constant inputs: argIn[3] = cell of Float64 input names,
    // argIn[4] = numeric vector of the constant values held for the whole run.
    std::vector<uint32_t> inVr;
    std::vector<double> inVals;
    if (argIn.size() >= 5) {
        if (!argIn[3].isCell()) {
            Error("Nelson:nflow_fmi:badInputs:inputNames",
                "inputNames must be a cell array of FMU input variable names.");
        }
        ArrayOf namesCell = argIn[3];
        const ArrayOf* nameElems = static_cast<const ArrayOf*>(namesCell.dataPointer());
        const size_t nIn = namesCell.dimensions().elementCount();
        const std::vector<double> vals = argIn[4].asDoubleVector();
        if (vals.size() != nIn) {
            Error("Nelson:nflow_fmi:badInputs:inputLength",
                "inputNames and inputValues must have the same length.");
        }
        for (size_t i = 0; i < nIn; ++i) {
            const std::string nm = wstring_to_utf8(nameElems[i].asWideString());
            const FmiVariable* found = nullptr;
            for (const FmiVariable& v : mi.variables) {
                if (v.causality == FmiCausality::Input && v.dataType == FmiDataType::Float64
                    && v.name == nm) {
                    found = &v;
                    break;
                }
            }
            if (found == nullptr) {
                RaiseError("unknown FMU Float64 input: '" + nm + "'.",
                    "Nelson:nflow_fmi:badInputs:unknownInput");
            }
            inVr.push_back(found->valueReference);
            inVals.push_back(vals[i]);
        }
    }

    std::unique_ptr<FmiInstance> inst
        = pkg->instantiateCoSimulation("nelson_cosim", false, errorMessage);
    if (!inst) {
        RaiseError(errorMessage, "Nelson:nflow_fmi:initializationFailed");
    }
    if (!inst->enterInitializationMode(relTol > 0.0, relTol, 0.0, true, tStop, errorMessage)) {
        RaiseError(errorMessage, "Nelson:nflow_fmi:initializationFailed");
    }
    if (!inVr.empty() && !inst->setFloat64(inVr, inVals, errorMessage)) {
        RaiseError(errorMessage, "Nelson:nflow_fmi:stepFailed");
    }
    if (!inst->exitInitializationMode(errorMessage)) {
        RaiseError(errorMessage, "Nelson:nflow_fmi:initializationFailed");
    }

    // Number of communication points (inclusive of t=0).
    const int nSteps = static_cast<int>(std::floor((tStop + 0.5 * dt) / dt));
    const size_t nRows = static_cast<size_t>(nSteps) + 1;

    std::vector<double> timeCol;
    timeCol.reserve(nRows);
    std::vector<double> outFlat; // column-major (nRows x nOut)
    outFlat.assign(nRows * nOut, 0.0);

    double t = 0.0;
    std::vector<double> values;
    for (size_t row = 0; row < nRows; ++row) {
        timeCol.push_back(t);
        if (nOut > 0) {
            if (!inst->getFloat64(outVr, values, errorMessage)) {
                RaiseError(errorMessage, "Nelson:nflow_fmi:outputReadFailed");
            }
            for (size_t j = 0; j < nOut; ++j) {
                outFlat[row + j * nRows] = values[j];
            }
        }
        if (row + 1 < nRows) {
            if (!inVr.empty() && !inst->setFloat64(inVr, inVals, errorMessage)) {
                RaiseError(errorMessage, "Nelson:nflow_fmi:stepFailed");
            }
            FmiStepResult step;
            if (!inst->doStep(t, dt, step, errorMessage)) {
                RaiseError(errorMessage, "Nelson:nflow_fmi:stepFailed");
            }
            if (step.earlyReturn) {
                Error(L"Nelson:nflow_fmi:earlyReturn",
                    L"fmiCoSimulate: FMU returned early at t=%s; variable communication steps are "
                    L"not supported yet.",
                    utf8_to_wstring(std::to_string(step.lastSuccessfulTime)));
            }
            t += dt;
            if (step.terminateRequested) {
                timeCol.resize(row + 2);
                timeCol.back() = t;
                break;
            }
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
