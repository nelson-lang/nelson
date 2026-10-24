//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef __EMSCRIPTEN__
#include "WasmFmuComponent.hpp"
#include <algorithm>
#include <emscripten.h>
#include <nlohmann/json.hpp>
//=============================================================================
EM_JS(int, nflowWasmFmuCall, (const char* request), {
    let response;
    try {
        response = Module['onNelsonFmu']
            ? Module['onNelsonFmu'](JSON.parse(UTF8ToString(request)))
            : { ok : false, error : 'FMI support is not available in this runtime.' };
    } catch (error) {
        response = { ok : false, error : String(error) };
    }
    Module['nflowFmuResponse'] = JSON.stringify(response);
    return lengthBytesUTF8(Module['nflowFmuResponse']) + 1;
});
//=============================================================================
EM_JS(void, nflowWasmFmuRead, (char* response, int size), {
    stringToUTF8(Module['nflowFmuResponse'], response, size);
    delete Module['nflowFmuResponse'];
});
//=============================================================================
namespace Nelson::NFlow {
using Json = nlohmann::json;
bool
invokeWasmFmu(const Json& request, Json& response, SimDiagnostic& diag)
{
    int size = nflowWasmFmuCall(request.dump().c_str());
    if (size < 1 || size > 8 * 1024 * 1024) {
        diag.code = "fmu_wasm_error";
        diag.message = "Invalid FMU response size.";
        diag.fatal = true;
        return false;
    }
    std::string raw(static_cast<size_t>(size), '\0');
    nflowWasmFmuRead(raw.data(), size);
    response = Json::parse(raw.c_str(), nullptr, false);
    if (!response.is_object() || !response.value("ok", false)) {
        diag.code = "fmu_wasm_error";
        diag.message = response.is_object() ? response.value("error", "FMU operation failed.")
                                            : "Invalid FMU response.";
        diag.fatal = true;
        return false;
    }
    diag.requestStop = response.value("requestStop", false);
    return true;
}
//=============================================================================
template <typename Interface> class WasmFmuBase : public Interface
{
public:
    explicit WasmFmuBase(const Json& response) : id_(response.at("id").get<int>())
    {
        setWidths(response);
    }
    ~WasmFmuBase() override
    {
        Json response;
        SimDiagnostic diag;
        call({ { "op", "destroy" } }, response, diag);
    }
    int
    numInputs() const override
    {
        return static_cast<int>(inputWidths_.size());
    }
    int
    numOutputs() const override
    {
        return static_cast<int>(outputWidths_.size());
    }
    int
    inputPortWidth(int p) const override
    {
        return inputWidths_.at(p);
    }
    int
    outputPortWidth(int p) const override
    {
        return outputWidths_.at(p);
    }
    void
    setSolverTolerance(double tolerance) override
    {
        tolerance_ = tolerance;
    }

    bool
    initialize(double startTime, double stopTime, const std::vector<std::string>& names,
        const std::vector<double>& values, SimDiagnostic& diag) override
    {
        Json response;
        if (!call({ { "op", "initialize" }, { "startTime", startTime }, { "stopTime", stopTime },
                      { "tolerance", tolerance_ }, { "names", names }, { "values", values } },
                response, diag)) {
            return false;
        }
        setWidths(response);
        initialStates_ = response.value("states", std::vector<double>());
        events_ = response.value("events", 0);
        return true;
    }
    bool
    setInputs(const double* values, int n, SimDiagnostic& diag) override
    {
        Json response;
        return call({ { "op", "inputs" }, { "values", vectorOf(values, n) } }, response, diag);
    }
    bool
    getOutputs(double* values, int n, SimDiagnostic& diag) override
    {
        return readValues("outputs", values, n, diag);
    }

protected:
    bool
    call(Json request, Json& response, SimDiagnostic& diag)
    {
        request["id"] = id_;
        return invokeWasmFmu(request, response, diag);
    }
    bool
    readValues(const char* op, double* values, int n, SimDiagnostic& diag)
    {
        Json response;
        if (!call({ { "op", op } }, response, diag)) {
            return false;
        }
        return copyValues(response, values, n, diag);
    }
    static bool
    copyValues(const Json& response, double* values, int n, SimDiagnostic& diag)
    {
        const auto output = response.at("values").get<std::vector<double>>();
        if (n != static_cast<int>(output.size())) {
            diag.code = "fmu_wasm_value_count";
            diag.message = "FMU value count mismatch.";
            diag.fatal = true;
            return false;
        }
        std::copy(output.begin(), output.end(), values);
        return true;
    }
    static std::vector<double>
    vectorOf(const double* values, int n)
    {
        return n > 0 ? std::vector<double>(values, values + n) : std::vector<double>();
    }
    void
    setWidths(const Json& response)
    {
        inputWidths_
            = response.value("inputWidths", std::vector<int>(response.value("inputs", 0), 1));
        outputWidths_
            = response.value("outputWidths", std::vector<int>(response.value("outputs", 0), 1));
    }
    int id_;
    int events_ = 0;
    std::vector<double> initialStates_;
    std::vector<int> inputWidths_;
    std::vector<int> outputWidths_;
    double tolerance_ = 1e-6;
};
//=============================================================================
class WasmFmuComponent final : public WasmFmuBase<IFmuComponent>
{
public:
    using WasmFmuBase::WasmFmuBase;
    bool
    configurePorts(const std::vector<std::string>& inputs, const std::vector<std::string>& outputs,
        SimDiagnostic& diag) override
    {
        Json response;
        if (!call({ { "op", "configure" }, { "inputs", inputs }, { "outputs", outputs } }, response,
                diag)) {
            return false;
        }
        setWidths(response);
        return true;
    }
    bool
    doStep(double time, double stepSize, SimDiagnostic& diag) override
    {
        Json response;
        return call(
            { { "op", "step" }, { "time", time }, { "stepSize", stepSize } }, response, diag);
    }
};
//=============================================================================
class WasmFmuMeComponent final : public WasmFmuBase<IFmuMeComponent>
{
public:
    using WasmFmuBase::WasmFmuBase;
    int
    numContinuousStates() const override
    {
        return static_cast<int>(initialStates_.size());
    }
    int
    numEventIndicators() const override
    {
        return events_;
    }
    void
    getInitialStates(double* x, int n) const override
    {
        std::copy_n(initialStates_.begin(), std::min(n, numContinuousStates()), x);
    }
    bool
    setStateTime(double time, const double* x, int n, SimDiagnostic& diag) override
    {
        Json response;
        return call(
            { { "op", "state" }, { "time", time }, { "values", vectorOf(x, n) } }, response, diag);
    }
    bool
    getDerivatives(double* values, int n, SimDiagnostic& diag) override
    {
        return readValues("derivatives", values, n, diag);
    }
    bool
    getEventIndicators(double* values, int n, SimDiagnostic& diag) override
    {
        return readValues("indicators", values, n, diag);
    }
    bool
    handleEvent(
        const double*, double* xout, int n, bool& statesChanged, SimDiagnostic& diag) override
    {
        Json response;
        if (!call({ { "op", "event" } }, response, diag)) {
            return false;
        }
        statesChanged = response.value("changed", false);
        return copyValues(response, xout, n, diag);
    }
};
//=============================================================================
std::unique_ptr<IFmuComponent>
makeWasmFmuComponent(const std::string& path, SimDiagnostic& diag)
{
    Json response;
    if (!invokeWasmFmu(
            { { "op", "create" }, { "path", path }, { "kind", "cs" } }, response, diag)) {
        return nullptr;
    }
    return std::make_unique<WasmFmuComponent>(response);
}
//=============================================================================
std::unique_ptr<IFmuMeComponent>
makeWasmFmuMeComponent(const std::string& path, SimDiagnostic& diag)
{
    Json response;
    if (!invokeWasmFmu(
            { { "op", "create" }, { "path", path }, { "kind", "me" } }, response, diag)) {
        return nullptr;
    }
    return std::make_unique<WasmFmuMeComponent>(response);
}
//=============================================================================
} // namespace Nelson::NFlow
#endif
//=============================================================================
