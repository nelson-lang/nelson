//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <cstdint>
#include <memory>
#include <string>
#include <vector>
#include "nlsNflow_fmi_exports.h"
//=============================================================================
namespace Nelson::NFlow::Fmi {
//=============================================================================
struct FmiStepResult
{
    bool eventEncountered = false;
    bool terminateRequested = false;
    bool earlyReturn = false;
    double lastSuccessfulTime = 0.0;
};
//=============================================================================
// RAII wrapper around an instantiated FMU (FMI 2.0 or 3.0, Co-Simulation or
// Model Exchange). The methods are version-neutral; the implementation branches
// on the version of the owning package.
//
// Created through FmiPackage::instantiate{CoSimulation,ModelExchange}(); the
// owning package must outlive the instance (fmi4c ties the instance to the
// loaded FMU). No fmi4c type appears in this header.
//=============================================================================
class NLSNFLOW_FMI_IMPEXP FmiInstance
{
public:
    // Adopts an opaque handle produced by the package: an fmi3InstanceHandle*
    // or, when fmi2 is true, an fmi2InstanceHandle*. Internal seam, callers use
    // FmiPackage::instantiate*. FMI 2.0 Model Exchange counts come from the FMU,
    // not the instance, so the package passes them in.
    static std::unique_ptr<FmiInstance>
    adopt(void* instanceHandle, bool fmi2 = false, int numContinuousStates = 0,
        int numEventIndicators = 0);

    ~FmiInstance();

    FmiInstance(const FmiInstance&) = delete;
    FmiInstance&
    operator=(const FmiInstance&)
        = delete;

    bool
    enterInitializationMode(bool toleranceDefined, double tolerance, double startTime,
        bool stopTimeDefined, double stopTime, std::string& errorMessage);
    bool
    exitInitializationMode(std::string& errorMessage);

    // Configuration Mode (between instantiate and enterInitializationMode):
    // the only state where structural parameters (e.g. array dimensions
    // bound to a <Dimension valueReference>) may be changed.
    bool
    enterConfigurationMode(std::string& errorMessage);
    bool
    exitConfigurationMode(std::string& errorMessage);

    bool
    setFloat64(const std::vector<uint32_t>& valueReferences, const std::vector<double>& values,
        std::string& errorMessage);
    bool
    getFloat64(const std::vector<uint32_t>& valueReferences, std::vector<double>& values,
        std::string& errorMessage);

    // Typed scalar access with double-backed conversion at the boundary.
    // `dataType` uses the FmiDataType numeric values (FmiPackage.hpp):
    // Float64/Float32, Int8..Int64, UInt8..UInt64, Boolean and Enumeration
    // (read/written as Int64). Values are converted to/from double; Int64
    // magnitudes beyond 2^53 lose precision. One VR at a time per value.
    bool
    setTyped(int dataType, const std::vector<uint32_t>& valueReferences,
        const std::vector<double>& values, std::string& errorMessage);
    // getTyped sizes `values` to nValues when nValues > 0 (FMI3 array form:
    // one value reference maps to N array elements); otherwise to the number
    // of value references.
    bool
    getTyped(int dataType, const std::vector<uint32_t>& valueReferences,
        std::vector<double>& values, std::string& errorMessage, int nValues = -1);

    // Advances one communication step. On success, result reflects the FMU's
    // reported flags. Fixed-step callers must reject earlyReturn unless they
    // can reschedule the communication grid to lastSuccessfulTime.
    bool
    doStep(double currentCommunicationPoint, double communicationStepSize, FmiStepResult& result,
        std::string& errorMessage);

    // Model Exchange lifecycle. The importing tool (NFlow) owns the solver: it
    // drives setTime + setContinuousStates, reads the derivatives and event
    // indicators, and handles events explicitly.
    size_t
    numberOfContinuousStates();
    size_t
    numberOfEventIndicators();
    bool
    enterContinuousTimeMode(std::string& errorMessage);
    bool
    enterEventMode(std::string& errorMessage);
    bool
    setTime(double time, std::string& errorMessage);
    bool
    setContinuousStates(const std::vector<double>& states, std::string& errorMessage);
    bool
    getContinuousStates(std::vector<double>& states, size_t n, std::string& errorMessage);
    bool
    getDerivatives(std::vector<double>& derivatives, size_t n, std::string& errorMessage);
    bool
    getEventIndicators(std::vector<double>& indicators, size_t n, std::string& errorMessage);
    // After each accepted integrator step. enterEvent is set when the FMU wants
    // event mode (time/step event).
    bool
    completedIntegratorStep(bool& enterEvent, bool& terminateRequested, std::string& errorMessage);
    // Runs the discrete-update fixed point in event mode. valuesChanged is set
    // when the continuous states must be re-read afterwards.
    bool
    updateDiscreteStates(bool& valuesChanged, bool& terminateRequested, std::string& errorMessage);

    bool
    terminate(std::string& errorMessage);

    // FMU state snapshot / rollback (needs canGetAndSetFMUState in the FMU).
    // saveState() captures the internal FMU state into one slot, restoreState()
    // rolls back to it. Both return false with a diagnostic when the FMU does
    // not support state get/set.
    bool
    saveState(std::string& errorMessage);
    bool
    restoreState(std::string& errorMessage);

private:
    FmiInstance();
    struct Impl;
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
} // namespace Nelson::NFlow::Fmi
//=============================================================================
