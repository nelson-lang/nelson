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
#include "FmiInspect.hpp"
#include "WasmFmuComponent.hpp"
#include <algorithm>
#include <stdexcept>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson::NFlow::Fmi {
using Json = nlohmann::json;
static bool
request(const Json& input, Json& output, std::string& error)
{
    SimDiagnostic diag;
    if (!invokeWasmFmu(input, output, diag)) {
        error = diag.message;
        return false;
    }
    return true;
}
//=============================================================================
struct FmiPackage::Impl
{
    std::string path;
    FmiModelInfo info;
};
//=============================================================================
struct FmiInstance::Impl
{
    int id = 0;
    bool
    call(const std::string& method, const Json& args, Json& output, std::string& error)
    {
        return request({ { "op", "fmi" }, { "id", id }, { "method", method }, { "args", args } },
            output, error);
    }
};
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
    return "FMI-LS-WASM component backend (CS and ME)";
}
//=============================================================================
FmiPackage::FmiPackage() : pImpl(std::make_unique<Impl>()) { }
FmiPackage::~FmiPackage() = default;
const FmiModelInfo&
FmiPackage::info() const
{
    return pImpl->info;
}
//=============================================================================
std::string
inspectFmuJson(const std::string& path, std::string& error)
{
    Json output;
    if (!request({ { "op", "inspect" }, { "path", path } }, output, error)) {
        return Json({ { "success", false }, { "error", error } }).dump();
    }
    return output.at("info").dump();
}
//=============================================================================
std::unique_ptr<FmiPackage>
FmiPackage::loadUnzipped(const std::string&, const std::string& path, std::string& error)
{
    const Json data = Json::parse(inspectFmuJson(path, error));
    if (!data.value("success", false)) {
        return nullptr;
    }
    auto pkg = std::unique_ptr<FmiPackage>(new FmiPackage());
    pkg->pImpl->path = path;
    auto& info = pkg->pImpl->info;
    info.modelIdentifier = data.value("modelIdentifier", "");
    info.fmiVersion = data.value("fmiVersion", "3.0");
    info.supportsCoSimulation = data.value("coSimulation", false);
    info.supportsModelExchange = data.value("modelExchange", false);
    const std::vector<std::string> causalities = { "parameter", "calculatedParameter", "input",
        "output", "local", "independent", "structuralParameter" };
    const std::vector<std::string> types
        = { "Float64", "Float32", "Int64", "Int32", "Int16", "Int8", "UInt64", "UInt32", "UInt16",
              "UInt8", "Boolean", "String", "Binary", "Clock", "Enumeration" };
    for (const auto& value : data.at("variables")) {
        FmiVariable variable;
        variable.name = value.at("name").get<std::string>();
        variable.valueReference = value.at("valueReference").get<uint32_t>();
        variable.description = value.value("description", "");
        variable.arraySize = value.value("arraySize", 1);
        const auto causality
            = std::find(causalities.begin(), causalities.end(), value.value("causality", ""));
        const auto type = std::find(types.begin(), types.end(), value.value("dataType", ""));
        variable.causality = static_cast<FmiCausality>(causality - causalities.begin());
        variable.dataType = static_cast<FmiDataType>(type - types.begin());
        variable.hasStartValue = value.contains("startValue");
        variable.startValue = value.value("startValue", 0.0);
        for (const auto& dim : value.value("dimensions", Json::array())) {
            variable.dimensions.push_back({ !dim.contains("vr"), dim.value("start", uint64_t(1)),
                dim.value("vr", uint32_t(0)) });
        }
        info.variables.push_back(variable);
    }
    return pkg;
}
//=============================================================================
static std::unique_ptr<FmiInstance>
instantiate(const std::string& path, const char* kind, std::string& error)
{
    Json output;
    if (!request({ { "op", "create" }, { "path", path }, { "kind", kind } }, output, error)) {
        return nullptr;
    }
    return FmiInstance::adopt(
        reinterpret_cast<void*>(static_cast<uintptr_t>(output.at("id").get<int>())));
}
//=============================================================================
std::unique_ptr<FmiInstance>
FmiPackage::instantiateCoSimulation(const std::string&, bool, std::string& error)
{
    return instantiate(pImpl->path, "cs", error);
}
//=============================================================================
std::unique_ptr<FmiInstance>
FmiPackage::instantiateModelExchange(const std::string&, bool, std::string& error)
{
    return instantiate(pImpl->path, "me", error);
}
//=============================================================================
int
effectiveArraySize(
    const FmiVariable& variable, const std::vector<std::pair<uint32_t, uint64_t>>& overrides)
{
    if (variable.dimensions.empty()) {
        return variable.arraySize;
    }
    uint64_t size = 1;
    for (const auto& dim : variable.dimensions) {
        uint64_t value = dim.start;
        for (const auto& overrideValue : overrides) {
            if (!dim.hasStart && dim.valueReference == overrideValue.first) {
                value = overrideValue.second;
            }
        }
        size *= value;
        if (size > 1000000) {
            throw std::runtime_error("FMU array exceeds 1000000 elements.");
        }
    }
    return static_cast<int>(size);
}
//=============================================================================
FmiInstance::FmiInstance() : pImpl(std::make_unique<Impl>()) { }
std::unique_ptr<FmiInstance>
FmiInstance::adopt(void* handle, bool, int, int)
{
    auto instance = std::unique_ptr<FmiInstance>(new FmiInstance());
    instance->pImpl->id = static_cast<int>(reinterpret_cast<uintptr_t>(handle));
    return instance;
}
//=============================================================================
FmiInstance::~FmiInstance()
{
    Json output;
    std::string error;
    request({ { "op", "destroy" }, { "id", pImpl->id } }, output, error);
}
#define WASM_FMI_TRANSITION(Name)                                                                  \
    bool FmiInstance::Name(std::string& error)                                                     \
    {                                                                                              \
        Json output;                                                                               \
        return pImpl->call(#Name, Json::array(), output, error);                                   \
    }
WASM_FMI_TRANSITION(exitInitializationMode)
WASM_FMI_TRANSITION(enterConfigurationMode)
WASM_FMI_TRANSITION(exitConfigurationMode)
WASM_FMI_TRANSITION(enterContinuousTimeMode)
WASM_FMI_TRANSITION(enterEventMode)
WASM_FMI_TRANSITION(terminate)
#undef WASM_FMI_TRANSITION
bool
FmiInstance::enterInitializationMode(bool toleranceDefined, double tolerance, double start,
    bool stopDefined, double stop, std::string& error)
{
    Json output;
    return pImpl->call("enterInitializationMode",
        Json::array({ toleranceDefined ? Json(tolerance) : Json(), start,
            stopDefined ? Json(stop) : Json() }),
        output, error);
}
//=============================================================================
bool
FmiInstance::setFloat64(
    const std::vector<uint32_t>& refs, const std::vector<double>& values, std::string& error)
{
    Json output;
    return pImpl->call("setFloat64", Json::array({ refs, values }), output, error);
}
//=============================================================================
bool
FmiInstance::getFloat64(
    const std::vector<uint32_t>& refs, std::vector<double>& values, std::string& error)
{
    Json output;
    if (!pImpl->call("getFloat64", Json::array({ refs }), output, error)) {
        return false;
    }
    values = output.at("value").get<std::vector<double>>();
    return true;
}
//=============================================================================
static std::string
typedMethod(int type)
{
    const std::vector<std::string> names = { "Float64", "Float32", "Int64", "Int32", "Int16",
        "Int8", "UInt64", "UInt32", "UInt16", "UInt8", "Boolean", "", "", "", "Int64" };
    return type >= 0 && type < static_cast<int>(names.size()) ? names[type] : "";
}
//=============================================================================
bool
FmiInstance::setTyped(int type, const std::vector<uint32_t>& refs,
    const std::vector<double>& values, std::string& error)
{
    Json output;
    return pImpl->call("set" + typedMethod(type), Json::array({ refs, values }), output, error);
}
//=============================================================================
bool
FmiInstance::getTyped(int type, const std::vector<uint32_t>& refs, std::vector<double>& values,
    std::string& error, int)
{
    Json output;
    if (!pImpl->call("get" + typedMethod(type), Json::array({ refs }), output, error)) {
        return false;
    }
    values = output.at("value").get<std::vector<double>>();
    return true;
}
#define WASM_FMI_VECTOR(Name, Method)                                                              \
    bool FmiInstance::Name(std::vector<double>& values, size_t, std::string& error)                \
    {                                                                                              \
        Json output;                                                                               \
        if (!pImpl->call(Method, Json::array(), output, error))                                    \
            return false;                                                                          \
        values = output.at("value").get<std::vector<double>>();                                    \
        return true;                                                                               \
    }
WASM_FMI_VECTOR(getContinuousStates, "getContinuousStates")
WASM_FMI_VECTOR(getDerivatives, "getContinuousStateDerivatives")
WASM_FMI_VECTOR(getEventIndicators, "getEventIndicators")
#undef WASM_FMI_VECTOR
size_t
FmiInstance::numberOfContinuousStates()
{
    Json output;
    std::string error;
    if (!pImpl->call("getNumberOfContinuousStates", Json::array(), output, error)) {
        throw std::runtime_error(error);
    }
    return output.at("value").get<size_t>();
}
//=============================================================================
size_t
FmiInstance::numberOfEventIndicators()
{
    Json output;
    std::string error;
    if (!pImpl->call("getNumberOfEventIndicators", Json::array(), output, error)) {
        throw std::runtime_error(error);
    }
    return output.at("value").get<size_t>();
}
//=============================================================================
bool
FmiInstance::setTime(double time, std::string& error)
{
    Json output;
    return pImpl->call("setTime", Json::array({ time }), output, error);
}
//=============================================================================
bool
FmiInstance::setContinuousStates(const std::vector<double>& states, std::string& error)
{
    Json output;
    return pImpl->call("setContinuousStates", Json::array({ states }), output, error);
}
//=============================================================================
bool
FmiInstance::doStep(double time, double step, FmiStepResult& result, std::string& error)
{
    Json output;
    if (!pImpl->call("doStep", Json::array({ time, step, false }), output, error)) {
        return false;
    }
    const auto& value = output.at("value");
    result.eventEncountered = value.value("eventHandlingNeeded", false);
    result.terminateRequested = value.value("terminateSimulation", false);
    result.earlyReturn = value.value("earlyReturn", false);
    result.lastSuccessfulTime = value.value("lastSuccessfulTime", time);
    return true;
}
//=============================================================================
bool
FmiInstance::completedIntegratorStep(bool& enterEvent, bool& terminateRequested, std::string& error)
{
    Json output;
    if (!pImpl->call("completedIntegratorStep", Json::array({ false }), output, error)) {
        return false;
    }
    enterEvent = output.at("value").value("enterEventMode", false);
    terminateRequested = output.at("value").value("terminateSimulation", false);
    return true;
}
//=============================================================================
bool
FmiInstance::updateDiscreteStates(bool& changed, bool& terminateRequested, std::string& error)
{
    Json output;
    if (!pImpl->call("updateDiscreteStates", Json::array(), output, error)) {
        return false;
    }
    changed = output.at("value").value("changed", false);
    terminateRequested = output.at("value").value("requestStop", false);
    return true;
}
//=============================================================================
bool
FmiInstance::saveState(std::string& error)
{
    error = "FMU state serialization is unavailable in this Wasm adapter.";
    return false;
}
//=============================================================================
bool
FmiInstance::restoreState(std::string& error)
{
    error = "FMU state serialization is unavailable in this Wasm adapter.";
    return false;
}
//=============================================================================
} // namespace Nelson::NFlow::Fmi
//=============================================================================
