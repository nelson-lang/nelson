//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FmiPackage.hpp"
#include "FmiInstance.hpp"
#include "fmi4c.h"
#include <cstdlib>
//=============================================================================
namespace Nelson::NFlow::Fmi {
//=============================================================================
// FMI 2.0 needs memory and logger callbacks from the caller (FMI 3.0 does not).
// The logger is a no-op: NFlow reports failures through return status.
static void
nflowFmi2Logger(fmi2ComponentEnvironment, fmi2String, fmi2Status, fmi2String, fmi2String, ...)
{
}
//=============================================================================
static void*
nflowFmi2AllocateMemory(size_t nobj, size_t size)
{
    return calloc(nobj, size);
}
//=============================================================================
static void
nflowFmi2FreeMemory(void* obj)
{
    free(obj);
}
//=============================================================================
// Which FMI standard the loaded package speaks; only the few divergent fmi4c
// calls branch on it.
enum class FmiVersion
{
    Fmi2,
    Fmi3
};
//=============================================================================
struct FmiPackage::Impl
{
    fmuHandle* fmu = nullptr;
    FmiVersion version = FmiVersion::Fmi3;
    FmiModelInfo info;

    ~Impl()
    {
        if (fmu != nullptr) {
            fmi4c_freeFmu(fmu);
            fmu = nullptr;
        }
    }
};
//=============================================================================
static FmiCausality
mapCausality(fmi3Causality c)
{
    switch (c) {
    case fmi3CausalityParameter:
        return FmiCausality::Parameter;
    case fmi3CausalityCalculatedParameter:
        return FmiCausality::CalculatedParameter;
    case fmi3CausalityInput:
        return FmiCausality::Input;
    case fmi3CausalityOutput:
        return FmiCausality::Output;
    case fmi3CausalityLocal:
        return FmiCausality::Local;
    case fmi3CausalityIndependent:
        return FmiCausality::Independent;
    case fmi3CausalityStructuralParameter:
        return FmiCausality::StructuralParameter;
    default:
        return FmiCausality::Unknown;
    }
}
//=============================================================================
static FmiDataType
mapDataType(fmi3DataType t)
{
    switch (t) {
    case fmi3DataTypeFloat64:
        return FmiDataType::Float64;
    case fmi3DataTypeFloat32:
        return FmiDataType::Float32;
    case fmi3DataTypeInt64:
        return FmiDataType::Int64;
    case fmi3DataTypeInt32:
        return FmiDataType::Int32;
    case fmi3DataTypeInt16:
        return FmiDataType::Int16;
    case fmi3DataTypeInt8:
        return FmiDataType::Int8;
    case fmi3DataTypeUInt64:
        return FmiDataType::UInt64;
    case fmi3DataTypeUInt32:
        return FmiDataType::UInt32;
    case fmi3DataTypeUInt16:
        return FmiDataType::UInt16;
    case fmi3DataTypeUInt8:
        return FmiDataType::UInt8;
    case fmi3DataTypeBoolean:
        return FmiDataType::Boolean;
    case fmi3DataTypeString:
        return FmiDataType::String;
    case fmi3DataTypeBinary:
        return FmiDataType::Binary;
    case fmi3DataTypeClock:
        return FmiDataType::Clock;
    case fmi3DataTypeEnumeration:
        return FmiDataType::Enumeration;
    default:
        return FmiDataType::Unknown;
    }
}
//=============================================================================
// FMI 2.0 has no structural parameters.
static FmiCausality
mapCausality2(fmi2Causality c)
{
    switch (c) {
    case fmi2CausalityParameter:
        return FmiCausality::Parameter;
    case fmi2CausalityCalculatedParameter:
        return FmiCausality::CalculatedParameter;
    case fmi2CausalityInput:
        return FmiCausality::Input;
    case fmi2CausalityOutput:
        return FmiCausality::Output;
    case fmi2CausalityLocal:
        return FmiCausality::Local;
    case fmi2CausalityIndependent:
        return FmiCausality::Independent;
    default:
        return FmiCausality::Unknown;
    }
}
//=============================================================================
// FMI 2.0 has four scalar types (Real/Integer/Boolean/String) plus Enumeration.
// The typed double-backed I/O path covers Real/Integer/Boolean; String is
// surfaced but not driven, as in FMI 3.0.
static FmiDataType
mapDataType2(fmi2DataType t)
{
    switch (t) {
    case fmi2DataTypeReal:
        return FmiDataType::Float64;
    case fmi2DataTypeInteger:
        return FmiDataType::Int32;
    case fmi2DataTypeBoolean:
        return FmiDataType::Boolean;
    case fmi2DataTypeString:
        return FmiDataType::String;
    case fmi2DataTypeEnumeration:
        return FmiDataType::Enumeration;
    default:
        return FmiDataType::Unknown;
    }
}
//=============================================================================
// All FMI 2.0 variables are scalar (no arrays, structural parameters or
// clocks), so the fmi3 dimension/clock handling is skipped.
static void
readModelInfoFmi2(fmuHandle* fmu, FmiModelInfo& mi)
{
    mi.fmiVersion = "2.0";
    mi.supportsCoSimulation = fmi2_getSupportsCoSimulation(fmu);
    mi.supportsModelExchange = fmi2_getSupportsModelExchange(fmu);
    mi.supportsScheduledExecution = false; // FMI 3.0 concept
    if (mi.supportsCoSimulation) {
        const char* id = fmi2cs_getModelIdentifier(fmu);
        mi.modelIdentifier = (id != nullptr) ? id : "";
        mi.canGetAndSetState = fmi2cs_getCanGetAndSetFMUState(fmu);
    } else if (mi.supportsModelExchange) {
        const char* id = fmi2me_getModelIdentifier(fmu);
        mi.modelIdentifier = (id != nullptr) ? id : "";
        mi.canGetAndSetState = fmi2me_getCanGetAndSetFMUState(fmu);
    }
    const int nVars = fmi2_getNumberOfVariables(fmu);
    mi.variables.reserve(nVars > 0 ? static_cast<size_t>(nVars) : 0);
    for (int i = 1; i <= nVars; ++i) { // fmi4c variable indices are 1-based
        fmi2VariableHandle* var = fmi2_getVariableByIndex(fmu, i);
        if (var == nullptr) {
            continue;
        }
        FmiVariable v;
        const char* name = fmi2_getVariableName(var);
        v.name = (name != nullptr) ? name : "";
        v.valueReference = static_cast<uint32_t>(fmi2_getVariableValueReference(var));
        v.causality = mapCausality2(fmi2_getVariableCausality(var));
        v.dataType = mapDataType2(fmi2_getVariableDataType(var));
        const char* desc = fmi2_getVariableDescription(var);
        v.description = (desc != nullptr) ? desc : "";
        v.hasStartValue = fmi2_getVariableHasStartValue(var);
        if (v.hasStartValue) {
            if (v.dataType == FmiDataType::Float64) {
                v.startValue = fmi2_getVariableStartReal(var);
            } else if (v.dataType == FmiDataType::Int32) {
                v.startValue = static_cast<double>(fmi2_getVariableStartInteger(var));
            }
        }
        v.arraySize = 1; // no array variables in FMI 2.0
        mi.variables.push_back(std::move(v));
    }
}
//=============================================================================
// Integer start value of a size variable (structural parameter used as an
// array dimension). Returns -1 when the variable has no usable integer start.
static long long
readIntegerStart(fmi3VariableHandle* var)
{
    if (var == nullptr || !fmi3_getVariableHasStartValue(var)) {
        return -1;
    }
    switch (fmi3_getVariableDataType(var)) {
    case fmi3DataTypeUInt64:
        return (long long)fmi3_getVariableStartUInt64(var);
    case fmi3DataTypeUInt32:
        return (long long)fmi3_getVariableStartUInt32(var);
    case fmi3DataTypeUInt16:
        return (long long)fmi3_getVariableStartUInt16(var);
    case fmi3DataTypeUInt8:
        return (long long)fmi3_getVariableStartUInt8(var);
    case fmi3DataTypeInt64:
        return (long long)fmi3_getVariableStartInt64(var);
    case fmi3DataTypeInt32:
        return (long long)fmi3_getVariableStartInt32(var);
    case fmi3DataTypeInt16:
        return (long long)fmi3_getVariableStartInt16(var);
    case fmi3DataTypeInt8:
        return (long long)fmi3_getVariableStartInt8(var);
    default:
        return -1;
    }
}
//=============================================================================
FmiPackage::FmiPackage() : pImpl(std::make_unique<Impl>()) { }
//=============================================================================
FmiPackage::~FmiPackage() = default;
//=============================================================================
std::unique_ptr<FmiPackage>
FmiPackage::loadUnzipped(
    const std::string& instanceName, const std::string& unzippedDir, std::string& errorMessage)
{
    errorMessage.clear();

    fmuHandle* fmu = fmi4c_loadUnzippedFmu(instanceName.c_str(), unzippedDir.c_str());
    if (fmu == nullptr) {
        errorMessage = "failed to load FMU from '" + unzippedDir
            + "' (not a valid FMU, or its modelDescription.xml is missing/invalid)";
        return nullptr;
    }

    const fmiVersion_t ver = fmi4c_getFmiVersion(fmu);
    if (ver != fmiVersion2 && ver != fmiVersion3) {
        fmi4c_freeFmu(fmu);
        errorMessage = "unsupported FMI version: only FMI 2.0 and 3.0 are supported "
                       "(FMI 1.0 is not supported; re-export the model as FMI 2.0 or 3.0)";
        return nullptr;
    }

    std::unique_ptr<FmiPackage> pkg(new FmiPackage());
    pkg->pImpl->fmu = fmu;
    pkg->pImpl->version = (ver == fmiVersion2) ? FmiVersion::Fmi2 : FmiVersion::Fmi3;

    FmiModelInfo& mi = pkg->pImpl->info;
    if (ver == fmiVersion2) {
        readModelInfoFmi2(fmu, mi);
        return pkg;
    }

    mi.fmiVersion = "3.0";
    mi.supportsCoSimulation = fmi3_supportsCoSimulation(fmu);
    mi.supportsModelExchange = fmi3_supportsModelExchange(fmu);
    mi.supportsScheduledExecution = fmi3_supportsScheduledExecution(fmu);
    if (mi.supportsCoSimulation) {
        const char* id = fmi3cs_getModelIdentifier(fmu);
        mi.modelIdentifier = (id != nullptr) ? id : "";
        mi.canGetAndSetState = fmi3cs_getCanGetAndSetFMUState(fmu);
    } else if (mi.supportsModelExchange) {
        const char* id = fmi3me_getModelIdentifier(fmu);
        mi.modelIdentifier = (id != nullptr) ? id : "";
        mi.canGetAndSetState = fmi3me_getCanGetAndSetFMUState(fmu);
    }

    const int nVars = fmi3_getNumberOfVariables(fmu);
    mi.variables.reserve(nVars > 0 ? static_cast<size_t>(nVars) : 0);
    for (int i = 1; i <= nVars; ++i) {
        fmi3VariableHandle* var = fmi3_getVariableByIndex(fmu, i);
        if (var == nullptr) {
            continue;
        }
        FmiVariable v;
        const char* name = fmi3_getVariableName(var);
        v.name = (name != nullptr) ? name : "";
        v.valueReference = static_cast<uint32_t>(fmi3_getVariableValueReference(var));
        v.causality = mapCausality(fmi3_getVariableCausality(var));
        v.dataType = mapDataType(fmi3_getVariableDataType(var));
        const char* desc = fmi3_getVariableDescription(var);
        v.description = (desc != nullptr) ? desc : "";
        v.hasStartValue = fmi3_getVariableHasStartValue(var);
        if (v.hasStartValue) {
            if (v.dataType == FmiDataType::Float64) {
                v.startValue = fmi3_getVariableStartFloat64(var);
            } else {
                // Integer-typed start (a structural parameter used as an array
                // dimension), surfaced as a double so the editor can seed the override.
                const long long iv = readIntegerStart(var);
                if (iv >= 0) {
                    v.startValue = static_cast<double>(iv);
                }
            }
        }
        // Array size = product of the dimension sizes. A dimension is a fixed size
        // (<Dimension start=...>) or bound to a structural parameter
        // (<Dimension valueReference=...>), resolved to its integer start value.
        const int nDim = fmi3_getVariableNumberOfDimensions(var);
        if (nDim > 0) {
            long long sz = 1;
            bool ok = true;
            for (int d = 0; d < nDim; ++d) {
                FmiDimension dim;
                if (fmi3_getVariableDimensionHasStart(var, d)) {
                    dim.hasStart = true;
                    dim.start = (uint64_t)fmi3_getVariableDimensionStart(var, d);
                    sz *= (long long)dim.start;
                } else {
                    const uint32_t dimVr = fmi3_getVariableDimensionValueReference(var, d);
                    dim.valueReference = dimVr;
                    fmi3VariableHandle* szVar = fmi3_getVariableByValueReference(fmu, dimVr);
                    if (szVar == nullptr) {
                        ok = false;
                        break;
                    }
                    long long dimSize = readIntegerStart(szVar);
                    if (dimSize < 0) {
                        ok = false;
                        break;
                    }
                    dim.start = (uint64_t)dimSize;
                    sz *= dimSize;
                }
                v.dimensions.push_back(dim);
            }
            if (ok && sz >= 1) {
                v.arraySize = (int)sz;
            } else {
                v.dimensions.clear();
            }
        }
        mi.variables.push_back(std::move(v));
    }

    // The simulation engine does not support Clock variables or Scheduled
    // Execution. Reject such FMUs here: inspect and co-simulation cannot drive
    // clock inputs.
    for (const FmiVariable& v : mi.variables) {
        if (v.dataType == FmiDataType::Clock) {
            errorMessage = "unsupported FMU: FMI 3.0 synchronous clocks (Clock "
                           "variables) are not supported";
            return nullptr;
        }
    }

    return pkg;
}
//=============================================================================
const FmiModelInfo&
FmiPackage::info() const
{
    return pImpl->info;
}
//=============================================================================
std::unique_ptr<FmiInstance>
FmiPackage::instantiateCoSimulation(
    const std::string& instanceName, bool loggingOn, std::string& errorMessage)
{
    errorMessage.clear();
    if (!pImpl->info.supportsCoSimulation) {
        errorMessage = "FMU does not support Co-Simulation";
        return nullptr;
    }
    if (pImpl->version == FmiVersion::Fmi2) {
        fmi2InstanceHandle* inst2 = fmi2_instantiate(pImpl->fmu, fmi2CoSimulation, nflowFmi2Logger,
            nflowFmi2AllocateMemory, nflowFmi2FreeMemory, nullptr, nullptr, fmi2False,
            loggingOn ? fmi2True : fmi2False);
        if (inst2 == nullptr) {
            errorMessage = "fmi2Instantiate (Co-Simulation) failed for '" + instanceName + "'";
            return nullptr;
        }
        return FmiInstance::adopt(inst2, true);
    }
    fmi3InstanceHandle* inst
        = fmi3_instantiateCoSimulation(pImpl->fmu, fmi3False, loggingOn ? fmi3True : fmi3False,
            fmi3False, fmi3False, nullptr, 0, nullptr, nullptr, nullptr);
    if (inst == nullptr) {
        errorMessage = "fmi3InstantiateCoSimulation failed for '" + instanceName + "'";
        return nullptr;
    }
    return FmiInstance::adopt(inst);
}
//=============================================================================
std::unique_ptr<FmiInstance>
FmiPackage::instantiateModelExchange(
    const std::string& instanceName, bool loggingOn, std::string& errorMessage)
{
    errorMessage.clear();
    if (!pImpl->info.supportsModelExchange) {
        errorMessage = "FMU does not support Model Exchange";
        return nullptr;
    }
    if (pImpl->version == FmiVersion::Fmi2) {
        fmi2InstanceHandle* inst2 = fmi2_instantiate(pImpl->fmu, fmi2ModelExchange, nflowFmi2Logger,
            nflowFmi2AllocateMemory, nflowFmi2FreeMemory, nullptr, nullptr, fmi2False,
            loggingOn ? fmi2True : fmi2False);
        if (inst2 == nullptr) {
            errorMessage = "fmi2Instantiate (Model Exchange) failed for '" + instanceName + "'";
            return nullptr;
        }
        // FMI 2.0 reads the state/indicator counts from the FMU, not the instance.
        const int nx = fmi2_getNumberOfContinuousStates(pImpl->fmu);
        const int nz = fmi2_getNumberOfEventIndicators(pImpl->fmu);
        return FmiInstance::adopt(inst2, true, nx, nz);
    }
    fmi3InstanceHandle* inst = fmi3_instantiateModelExchange(
        pImpl->fmu, fmi3False, loggingOn ? fmi3True : fmi3False, nullptr, nullptr);
    if (inst == nullptr) {
        errorMessage = "fmi3InstantiateModelExchange failed for '" + instanceName + "'";
        return nullptr;
    }
    return FmiInstance::adopt(inst);
}
//=============================================================================
int
effectiveArraySize(
    const FmiVariable& v, const std::vector<std::pair<uint32_t, uint64_t>>& overridesByVr)
{
    if (v.dimensions.empty()) {
        return (std::max)(1, v.arraySize);
    }
    long long sz = 1;
    for (const FmiDimension& d : v.dimensions) {
        uint64_t extent = d.start;
        if (!d.hasStart) {
            for (const auto& kv : overridesByVr) {
                if (kv.first == d.valueReference) {
                    extent = kv.second;
                    break;
                }
            }
        }
        sz *= (long long)extent;
    }
    return (sz >= 1) ? (int)sz : 1;
}
//=============================================================================
bool
fmiSupportCompiledIn()
{
    return true;
}
//=============================================================================
std::string
fmi4cBuildInfo()
{
    return "NFLOW - FMI 2.0 & 3.0 import";
}
//=============================================================================
} // namespace Nelson::NFlow::Fmi
//=============================================================================
