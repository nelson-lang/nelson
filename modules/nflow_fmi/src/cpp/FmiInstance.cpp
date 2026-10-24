//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FmiInstance.hpp"
#include "FmiPackage.hpp" // FmiDataType numeric values for setTyped/getTyped
#include "fmi4c.h"
#include <cmath>
//=============================================================================
namespace Nelson::NFlow::Fmi {
//=============================================================================
struct FmiInstance::Impl
{
    fmi3InstanceHandle* instance = nullptr; // FMI 3.0 handle (isV2 == false)
    fmi2InstanceHandle* instance2 = nullptr; // FMI 2.0 handle (isV2 == true)
    bool isV2 = false;
    bool terminated = false;
    int nx2 = 0; // FMI 2.0 Model Exchange: continuous-state count (from the FMU)
    int nz2 = 0; // FMI 2.0 Model Exchange: event-indicator count (from the FMU)
    void* savedState = nullptr; // last snapshot (fmi2FMUstate / fmi3FMUState)

    ~Impl()
    {
        if (isV2) {
            if (savedState != nullptr && instance2 != nullptr) {
                fmi2_freeFMUstate(instance2, &savedState);
            }
            if (instance2 != nullptr) {
                fmi2_freeInstance(instance2);
                instance2 = nullptr;
            }
        } else {
            if (savedState != nullptr && instance != nullptr) {
                fmi3_freeFMUState(instance, &savedState);
            }
            if (instance != nullptr) {
                fmi3_freeInstance(instance);
                instance = nullptr;
            }
        }
        savedState = nullptr;
    }
};
//=============================================================================
FmiInstance::FmiInstance() : pImpl(std::make_unique<Impl>()) { }
//=============================================================================
FmiInstance::~FmiInstance() = default;
//=============================================================================
std::unique_ptr<FmiInstance>
FmiInstance::adopt(void* instanceHandle, bool fmi2, int numContinuousStates, int numEventIndicators)
{
    if (instanceHandle == nullptr) {
        return nullptr;
    }
    std::unique_ptr<FmiInstance> self(new FmiInstance());
    self->pImpl->isV2 = fmi2;
    self->pImpl->nx2 = numContinuousStates;
    self->pImpl->nz2 = numEventIndicators;
    if (fmi2) {
        self->pImpl->instance2 = static_cast<fmi2InstanceHandle*>(instanceHandle);
    } else {
        self->pImpl->instance = static_cast<fmi3InstanceHandle*>(instanceHandle);
    }
    return self;
}
//=============================================================================
static bool
statusOk(fmi3Status status)
{
    return status == fmi3OK || status == fmi3Warning;
}
//=============================================================================
static bool
statusOk2(fmi2Status status)
{
    return status == fmi2OK || status == fmi2Warning;
}
//=============================================================================
bool
FmiInstance::enterInitializationMode(bool toleranceDefined, double tolerance, double startTime,
    bool stopTimeDefined, double stopTime, std::string& errorMessage)
{
    if (pImpl->isV2) {
        // FMI 2.0 splits what fmi3 folds into enterInitializationMode: the
        // experiment setup (tolerance/start/stop) comes first, then enter-init.
        if (!statusOk2(fmi2_setupExperiment(pImpl->instance2, toleranceDefined, tolerance,
                startTime, stopTimeDefined, stopTime))) {
            errorMessage = "fmi2SetupExperiment failed";
            return false;
        }
        if (!statusOk2(fmi2_enterInitializationMode(pImpl->instance2))) {
            errorMessage = "fmi2EnterInitializationMode failed";
            return false;
        }
        return true;
    }
    const fmi3Status s = fmi3_enterInitializationMode(
        pImpl->instance, toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime);
    if (!statusOk(s)) {
        errorMessage = "fmi3EnterInitializationMode failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::exitInitializationMode(std::string& errorMessage)
{
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_exitInitializationMode(pImpl->instance2))) {
            errorMessage = "fmi2ExitInitializationMode failed";
            return false;
        }
        return true;
    }
    const fmi3Status s = fmi3_exitInitializationMode(pImpl->instance);
    if (!statusOk(s)) {
        errorMessage = "fmi3ExitInitializationMode failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::enterConfigurationMode(std::string& errorMessage)
{
    if (pImpl->isV2) {
        return true; // no Configuration Mode in FMI 2.0 (no structural parameters)
    }
    const fmi3Status s = fmi3_enterConfigurationMode(pImpl->instance);
    if (!statusOk(s)) {
        errorMessage = "fmi3EnterConfigurationMode failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::exitConfigurationMode(std::string& errorMessage)
{
    if (pImpl->isV2) {
        return true; // no Configuration Mode in FMI 2.0
    }
    const fmi3Status s = fmi3_exitConfigurationMode(pImpl->instance);
    if (!statusOk(s)) {
        errorMessage = "fmi3ExitConfigurationMode failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::setFloat64(const std::vector<uint32_t>& valueReferences,
    const std::vector<double>& values, std::string& errorMessage)
{
    // FMI3 array form: one value reference may map to N array elements, so
    // values may exceed the VR count; it can never be shorter.
    if (values.size() < valueReferences.size()) {
        errorMessage = "setFloat64: value-reference / value count mismatch";
        return false;
    }
    if (valueReferences.empty()) {
        return true;
    }
    if (pImpl->isV2) {
        std::vector<fmi2ValueReference> vr(valueReferences.begin(), valueReferences.end());
        if (!statusOk2(fmi2_setReal(pImpl->instance2, vr.data(), vr.size(), values.data()))) {
            errorMessage = "fmi2SetReal failed";
            return false;
        }
        return true;
    }
    std::vector<fmi3ValueReference> vr(valueReferences.begin(), valueReferences.end());
    const fmi3Status s
        = fmi3_setFloat64(pImpl->instance, vr.data(), vr.size(), values.data(), values.size());
    if (!statusOk(s)) {
        errorMessage = "fmi3SetFloat64 failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::getFloat64(const std::vector<uint32_t>& valueReferences, std::vector<double>& values,
    std::string& errorMessage)
{
    values.assign(valueReferences.size(), 0.0);
    if (valueReferences.empty()) {
        return true;
    }
    if (pImpl->isV2) {
        std::vector<fmi2ValueReference> vr(valueReferences.begin(), valueReferences.end());
        if (!statusOk2(fmi2_getReal(pImpl->instance2, vr.data(), vr.size(), values.data()))) {
            errorMessage = "fmi2GetReal failed";
            return false;
        }
        return true;
    }
    std::vector<fmi3ValueReference> vr(valueReferences.begin(), valueReferences.end());
    const fmi3Status s
        = fmi3_getFloat64(pImpl->instance, vr.data(), vr.size(), values.data(), values.size());
    if (!statusOk(s)) {
        errorMessage = "fmi3GetFloat64 failed";
        return false;
    }
    return true;
}
//=============================================================================
template <typename T, typename SetFn>
static bool
setAs(fmi3InstanceHandle* inst, SetFn fn, const std::vector<fmi3ValueReference>& vr,
    const std::vector<double>& values)
{
    std::vector<T> tv(values.size());
    for (size_t i = 0; i < values.size(); ++i) {
        tv[i] = static_cast<T>(std::llround(values[i]));
    }
    return statusOk(fn(inst, vr.data(), vr.size(), tv.data(), tv.size()));
}
//=============================================================================
template <typename T, typename GetFn>
static bool
getAs(fmi3InstanceHandle* inst, GetFn fn, const std::vector<fmi3ValueReference>& vr,
    std::vector<double>& values)
{
    // values is pre-sized to the expected element count (nValues, which may
    // exceed vr.size() for an array-valued variable).
    std::vector<T> tv(values.size(), static_cast<T>(0));
    if (!statusOk(fn(inst, vr.data(), vr.size(), tv.data(), tv.size()))) {
        return false;
    }
    for (size_t i = 0; i < tv.size(); ++i) {
        values[i] = static_cast<double>(tv[i]);
    }
    return true;
}
//=============================================================================
bool
FmiInstance::setTyped(int dataType, const std::vector<uint32_t>& valueReferences,
    const std::vector<double>& values, std::string& errorMessage)
{
    // FMI3 array form: one value reference may map to N array elements, so
    // values may exceed the VR count; it can never be shorter.
    if (values.size() < valueReferences.size()) {
        errorMessage = "setTyped: value-reference / value count mismatch";
        return false;
    }
    if (valueReferences.empty()) {
        return true;
    }
    if (pImpl->isV2) {
        // FMI 2.0 scalar types: Real (Float64), Integer (Int32/Enumeration),
        // Boolean. Nothing else can appear (mapDataType2).
        const FmiDataType dt = static_cast<FmiDataType>(dataType);
        if (dt == FmiDataType::Float64) {
            return setFloat64(valueReferences, values, errorMessage);
        }
        std::vector<fmi2ValueReference> vr(valueReferences.begin(), valueReferences.end());
        if (dt == FmiDataType::Int32 || dt == FmiDataType::Enumeration) {
            std::vector<fmi2Integer> tv(values.size());
            for (size_t i = 0; i < values.size(); ++i) {
                tv[i] = static_cast<fmi2Integer>(std::llround(values[i]));
            }
            if (!statusOk2(fmi2_setInteger(pImpl->instance2, vr.data(), vr.size(), tv.data()))) {
                errorMessage = "fmi2SetInteger failed";
                return false;
            }
            return true;
        }
        if (dt == FmiDataType::Boolean) {
            std::vector<fmi2Boolean> tv(values.size());
            for (size_t i = 0; i < values.size(); ++i) {
                tv[i] = (!std::isnan(values[i]) && values[i] != 0.0) ? fmi2True : fmi2False;
            }
            if (!statusOk2(fmi2_setBoolean(pImpl->instance2, vr.data(), vr.size(), tv.data()))) {
                errorMessage = "fmi2SetBoolean failed";
                return false;
            }
            return true;
        }
        errorMessage = "setTyped: unsupported FMI 2.0 data type";
        return false;
    }
    std::vector<fmi3ValueReference> vr(valueReferences.begin(), valueReferences.end());
    auto* inst = pImpl->instance;
    bool ok = false;
    switch (static_cast<FmiDataType>(dataType)) {
    case FmiDataType::Float64:
        return setFloat64(valueReferences, values, errorMessage);
    case FmiDataType::Float32: {
        std::vector<fmi3Float32> tv(values.size());
        for (size_t i = 0; i < values.size(); ++i) {
            tv[i] = static_cast<fmi3Float32>(values[i]);
        }
        ok = statusOk(fmi3_setFloat32(inst, vr.data(), vr.size(), tv.data(), tv.size()));
        break;
    }
    case FmiDataType::Int8:
        ok = setAs<fmi3Int8>(inst, fmi3_setInt8, vr, values);
        break;
    case FmiDataType::UInt8:
        ok = setAs<fmi3UInt8>(inst, fmi3_setUInt8, vr, values);
        break;
    case FmiDataType::Int16:
        ok = setAs<fmi3Int16>(inst, fmi3_setInt16, vr, values);
        break;
    case FmiDataType::UInt16:
        ok = setAs<fmi3UInt16>(inst, fmi3_setUInt16, vr, values);
        break;
    case FmiDataType::Int32:
        ok = setAs<fmi3Int32>(inst, fmi3_setInt32, vr, values);
        break;
    case FmiDataType::UInt32:
        ok = setAs<fmi3UInt32>(inst, fmi3_setUInt32, vr, values);
        break;
    case FmiDataType::Int64:
    case FmiDataType::Enumeration:
        ok = setAs<fmi3Int64>(inst, fmi3_setInt64, vr, values);
        break;
    case FmiDataType::UInt64:
        ok = setAs<fmi3UInt64>(inst, fmi3_setUInt64, vr, values);
        break;
    case FmiDataType::Boolean: {
        // fmi3Boolean may alias bool (vector<bool> has no data()): use a
        // plain buffer.
        std::unique_ptr<fmi3Boolean[]> tv(new fmi3Boolean[values.size()]);
        for (size_t i = 0; i < values.size(); ++i) {
            tv[i] = (!std::isnan(values[i]) && values[i] != 0.0) ? fmi3True : fmi3False;
        }
        ok = statusOk(fmi3_setBoolean(inst, vr.data(), vr.size(), tv.get(), values.size()));
        break;
    }
    default:
        errorMessage = "setTyped: unsupported FMI data type";
        return false;
    }
    if (!ok) {
        errorMessage = "typed fmi3Set* failed";
    }
    return ok;
}
//=============================================================================
bool
FmiInstance::getTyped(int dataType, const std::vector<uint32_t>& valueReferences,
    std::vector<double>& values, std::string& errorMessage, int nValues)
{
    const size_t n = (nValues > 0) ? static_cast<size_t>(nValues) : valueReferences.size();
    values.assign(n, 0.0);
    if (valueReferences.empty()) {
        return true;
    }
    if (pImpl->isV2) {
        // FMI 2.0: one VR = one scalar (never an array), so n == vr count.
        const FmiDataType dt = static_cast<FmiDataType>(dataType);
        if (dt == FmiDataType::Float64) {
            return getFloat64(valueReferences, values, errorMessage);
        }
        std::vector<fmi2ValueReference> vr(valueReferences.begin(), valueReferences.end());
        if (dt == FmiDataType::Int32 || dt == FmiDataType::Enumeration) {
            std::vector<fmi2Integer> tv(vr.size(), 0);
            if (!statusOk2(fmi2_getInteger(pImpl->instance2, vr.data(), vr.size(), tv.data()))) {
                errorMessage = "fmi2GetInteger failed";
                return false;
            }
            for (size_t i = 0; i < tv.size(); ++i) {
                values[i] = static_cast<double>(tv[i]);
            }
            return true;
        }
        if (dt == FmiDataType::Boolean) {
            std::vector<fmi2Boolean> tv(vr.size(), fmi2False);
            if (!statusOk2(fmi2_getBoolean(pImpl->instance2, vr.data(), vr.size(), tv.data()))) {
                errorMessage = "fmi2GetBoolean failed";
                return false;
            }
            for (size_t i = 0; i < tv.size(); ++i) {
                values[i] = (tv[i] != fmi2False) ? 1.0 : 0.0;
            }
            return true;
        }
        errorMessage = "getTyped: unsupported FMI 2.0 data type";
        return false;
    }
    std::vector<fmi3ValueReference> vr(valueReferences.begin(), valueReferences.end());
    auto* inst = pImpl->instance;
    bool ok = false;
    switch (static_cast<FmiDataType>(dataType)) {
    case FmiDataType::Float64: {
        ok = statusOk(fmi3_getFloat64(inst, vr.data(), vr.size(), values.data(), values.size()));
        break;
    }
    case FmiDataType::Float32: {
        std::vector<fmi3Float32> tv(values.size(), 0.0f);
        ok = statusOk(fmi3_getFloat32(inst, vr.data(), vr.size(), tv.data(), tv.size()));
        if (ok) {
            for (size_t i = 0; i < tv.size(); ++i) {
                values[i] = static_cast<double>(tv[i]);
            }
        }
        break;
    }
    case FmiDataType::Int8:
        ok = getAs<fmi3Int8>(inst, fmi3_getInt8, vr, values);
        break;
    case FmiDataType::UInt8:
        ok = getAs<fmi3UInt8>(inst, fmi3_getUInt8, vr, values);
        break;
    case FmiDataType::Int16:
        ok = getAs<fmi3Int16>(inst, fmi3_getInt16, vr, values);
        break;
    case FmiDataType::UInt16:
        ok = getAs<fmi3UInt16>(inst, fmi3_getUInt16, vr, values);
        break;
    case FmiDataType::Int32:
        ok = getAs<fmi3Int32>(inst, fmi3_getInt32, vr, values);
        break;
    case FmiDataType::UInt32:
        ok = getAs<fmi3UInt32>(inst, fmi3_getUInt32, vr, values);
        break;
    case FmiDataType::Int64:
    case FmiDataType::Enumeration:
        ok = getAs<fmi3Int64>(inst, fmi3_getInt64, vr, values);
        break;
    case FmiDataType::UInt64:
        ok = getAs<fmi3UInt64>(inst, fmi3_getUInt64, vr, values);
        break;
    case FmiDataType::Boolean: {
        // fmi3Boolean may alias bool (vector<bool> has no data()): use a
        // plain buffer sized to the element count (array-aware).
        const size_t nv = values.size();
        std::unique_ptr<fmi3Boolean[]> tv(new fmi3Boolean[nv]);
        for (size_t i = 0; i < nv; ++i) {
            tv[i] = fmi3False;
        }
        ok = statusOk(fmi3_getBoolean(inst, vr.data(), vr.size(), tv.get(), nv));
        if (ok) {
            for (size_t i = 0; i < nv; ++i) {
                values[i] = tv[i] ? 1.0 : 0.0;
            }
        }
        break;
    }
    default:
        errorMessage = "getTyped: unsupported FMI data type";
        return false;
    }
    if (!ok && errorMessage.empty()) {
        errorMessage = "typed fmi3Get* failed";
    }
    return ok;
}
//=============================================================================
bool
FmiInstance::doStep(double currentCommunicationPoint, double communicationStepSize,
    FmiStepResult& result, std::string& errorMessage)
{
    result = FmiStepResult {};
    result.lastSuccessfulTime = currentCommunicationPoint + communicationStepSize;
    if (pImpl->isV2) {
        // FMI 2.0 CS has no event out-params. A discarded step is the FMU
        // requesting to stop (e.g. it detected a terminal condition).
        const fmi2Status s = fmi2_doStep(
            pImpl->instance2, currentCommunicationPoint, communicationStepSize, fmi2True);
        if (s == fmi2Discard) {
            result.terminateRequested = true;
            return true;
        }
        if (!statusOk2(s)) {
            errorMessage = "fmi2DoStep failed";
            return false;
        }
        return true;
    }
    fmi3Boolean fmiEvent = false;
    fmi3Boolean fmiTerminate = false;
    fmi3Boolean earlyReturn = false;
    fmi3Float64 lastSuccessfulTime = 0.0;
    const fmi3Status s
        = fmi3_doStep(pImpl->instance, currentCommunicationPoint, communicationStepSize, fmi3True,
            &fmiEvent, &fmiTerminate, &earlyReturn, &lastSuccessfulTime);
    result.eventEncountered = fmiEvent;
    result.terminateRequested = fmiTerminate;
    result.earlyReturn = earlyReturn;
    result.lastSuccessfulTime = lastSuccessfulTime;
    if (!statusOk(s)) {
        errorMessage = "fmi3DoStep failed";
        return false;
    }
    return true;
}
//=============================================================================
size_t
FmiInstance::numberOfContinuousStates()
{
    if (pImpl->isV2) {
        return pImpl->nx2 > 0 ? static_cast<size_t>(pImpl->nx2) : 0;
    }
    size_t n = 0;
    if (!statusOk(fmi3_getNumberOfContinuousStates(pImpl->instance, &n))) {
        return 0;
    }
    return n;
}
//=============================================================================
size_t
FmiInstance::numberOfEventIndicators()
{
    if (pImpl->isV2) {
        return pImpl->nz2 > 0 ? static_cast<size_t>(pImpl->nz2) : 0;
    }
    size_t n = 0;
    if (!statusOk(fmi3_getNumberOfEventIndicators(pImpl->instance, &n))) {
        return 0;
    }
    return n;
}
//=============================================================================
bool
FmiInstance::enterContinuousTimeMode(std::string& errorMessage)
{
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_enterContinuousTimeMode(pImpl->instance2))) {
            errorMessage = "fmi2EnterContinuousTimeMode failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_enterContinuousTimeMode(pImpl->instance))) {
        errorMessage = "fmi3EnterContinuousTimeMode failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::enterEventMode(std::string& errorMessage)
{
    if (pImpl->isV2) {
        // FMI 2.0 enterEventMode takes no argument (v3 passes an event qualifier).
        if (!statusOk2(fmi2_enterEventMode(pImpl->instance2))) {
            errorMessage = "fmi2EnterEventMode failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_enterEventMode(pImpl->instance))) {
        errorMessage = "fmi3EnterEventMode failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::setTime(double time, std::string& errorMessage)
{
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_setTime(pImpl->instance2, time))) {
            errorMessage = "fmi2SetTime failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_setTime(pImpl->instance, time))) {
        errorMessage = "fmi3SetTime failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::setContinuousStates(const std::vector<double>& states, std::string& errorMessage)
{
    if (states.empty()) {
        return true;
    }
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_setContinuousStates(pImpl->instance2, states.data(), states.size()))) {
            errorMessage = "fmi2SetContinuousStates failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_setContinuousStates(pImpl->instance, states.data(), states.size()))) {
        errorMessage = "fmi3SetContinuousStates failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::getContinuousStates(std::vector<double>& states, size_t n, std::string& errorMessage)
{
    states.assign(n, 0.0);
    if (n == 0) {
        return true;
    }
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_getContinuousStates(pImpl->instance2, states.data(), n))) {
            errorMessage = "fmi2GetContinuousStates failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_getContinuousStates(pImpl->instance, states.data(), n))) {
        errorMessage = "fmi3GetContinuousStates failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::getDerivatives(std::vector<double>& derivatives, size_t n, std::string& errorMessage)
{
    derivatives.assign(n, 0.0);
    if (n == 0) {
        return true;
    }
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_getDerivatives(pImpl->instance2, derivatives.data(), n))) {
            errorMessage = "fmi2GetDerivatives failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_getContinuousStateDerivatives(pImpl->instance, derivatives.data(), n))) {
        errorMessage = "fmi3GetContinuousStateDerivatives failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::getEventIndicators(
    std::vector<double>& indicators, size_t n, std::string& errorMessage)
{
    indicators.assign(n, 0.0);
    if (n == 0) {
        return true;
    }
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_getEventIndicators(pImpl->instance2, indicators.data(), n))) {
            errorMessage = "fmi2GetEventIndicators failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_getEventIndicators(pImpl->instance, indicators.data(), n))) {
        errorMessage = "fmi3GetEventIndicators failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::completedIntegratorStep(
    bool& enterEvent, bool& terminateRequested, std::string& errorMessage)
{
    if (pImpl->isV2) {
        fmi2Boolean enterEventMode2 = fmi2False;
        fmi2Boolean terminateSimulation2 = fmi2False;
        if (!statusOk2(fmi2_completedIntegratorStep(
                pImpl->instance2, fmi2True, &enterEventMode2, &terminateSimulation2))) {
            errorMessage = "fmi2CompletedIntegratorStep failed";
            return false;
        }
        enterEvent = (enterEventMode2 != fmi2False);
        terminateRequested = (terminateSimulation2 != fmi2False);
        return true;
    }
    fmi3Boolean enterEventMode = false;
    fmi3Boolean terminateSimulation = false;
    if (!statusOk(fmi3_completedIntegratorStep(
            pImpl->instance, fmi3True, &enterEventMode, &terminateSimulation))) {
        errorMessage = "fmi3CompletedIntegratorStep failed";
        return false;
    }
    enterEvent = enterEventMode;
    terminateRequested = terminateSimulation;
    return true;
}
//=============================================================================
bool
FmiInstance::updateDiscreteStates(
    bool& valuesChanged, bool& terminateRequested, std::string& errorMessage)
{
    valuesChanged = false;
    terminateRequested = false;
    if (pImpl->isV2) {
        // FMI 2.0: iterate fmi2_newDiscreteStates until no further update is
        // needed (the fixed point v3 folds into updateDiscreteStates out-params).
        for (int guard = 0; guard < 10000; ++guard) {
            fmi2EventInfo ei;
            ei.newDiscreteStatesNeeded = fmi2False;
            ei.terminateSimulation = fmi2False;
            ei.nominalsOfContinuousStatesChanged = fmi2False;
            ei.valuesOfContinuousStatesChanged = fmi2False;
            ei.nextEventTimeDefined = fmi2False;
            ei.nextEventTime = 0.0;
            if (!statusOk2(fmi2_newDiscreteStates(pImpl->instance2, &ei))) {
                errorMessage = "fmi2NewDiscreteStates failed";
                return false;
            }
            if (ei.valuesOfContinuousStatesChanged != fmi2False) {
                valuesChanged = true;
            }
            if (ei.terminateSimulation != fmi2False) {
                terminateRequested = true;
                return true;
            }
            if (ei.newDiscreteStatesNeeded == fmi2False) {
                break;
            }
        }
        return true;
    }
    for (int guard = 0; guard < 10000; ++guard) {
        fmi3Boolean discreteStatesNeedUpdate = false;
        fmi3Boolean terminateSimulation = false;
        fmi3Boolean nominalsChanged = false;
        fmi3Boolean valuesOfContinuousStatesChanged = false;
        fmi3Boolean nextEventTimeDefined = false;
        fmi3Float64 nextEventTime = 0.0;
        if (!statusOk(fmi3_updateDiscreteStates(pImpl->instance, &discreteStatesNeedUpdate,
                &terminateSimulation, &nominalsChanged, &valuesOfContinuousStatesChanged,
                &nextEventTimeDefined, &nextEventTime))) {
            errorMessage = "fmi3UpdateDiscreteStates failed";
            return false;
        }
        if (valuesOfContinuousStatesChanged) {
            valuesChanged = true;
        }
        if (terminateSimulation) {
            terminateRequested = true;
            return true;
        }
        if (!discreteStatesNeedUpdate) {
            break;
        }
    }
    return true;
}
//=============================================================================
bool
FmiInstance::terminate(std::string& errorMessage)
{
    if (pImpl->terminated) {
        return true;
    }
    pImpl->terminated = true;
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_terminate(pImpl->instance2))) {
            errorMessage = "fmi2Terminate failed";
            return false;
        }
        return true;
    }
    const fmi3Status s = fmi3_terminate(pImpl->instance);
    if (!statusOk(s)) {
        errorMessage = "fmi3Terminate failed";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::saveState(std::string& errorMessage)
{
    // getFMUstate reuses the slot when *state != nullptr, so a repeated save
    // does not leak.
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_getFMUstate(pImpl->instance2, &pImpl->savedState))) {
            errorMessage = "fmi2GetFMUstate failed (FMU may not support "
                           "canGetAndSetFMUstate)";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_getFMUState(pImpl->instance, &pImpl->savedState))) {
        errorMessage = "fmi3GetFMUState failed (FMU may not support "
                       "canGetAndSetFMUState)";
        return false;
    }
    return true;
}
//=============================================================================
bool
FmiInstance::restoreState(std::string& errorMessage)
{
    if (pImpl->savedState == nullptr) {
        errorMessage = "restoreState: no state has been saved";
        return false;
    }
    if (pImpl->isV2) {
        if (!statusOk2(fmi2_setFMUstate(pImpl->instance2, pImpl->savedState))) {
            errorMessage = "fmi2SetFMUstate failed";
            return false;
        }
        return true;
    }
    if (!statusOk(fmi3_setFMUState(pImpl->instance, pImpl->savedState))) {
        errorMessage = "fmi3SetFMUState failed";
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson::NFlow::Fmi
//=============================================================================
