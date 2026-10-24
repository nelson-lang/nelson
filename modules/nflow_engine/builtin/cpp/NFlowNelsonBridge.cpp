//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include <cmath>
#include <mutex>
#include <thread>
#include "NFlowNelsonBridge.hpp"
#include "NFlowWorkspaceIO.hpp"
#include "NelsonWorkspaceBridge.hpp"
#include "ArrayOf.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "i18n.hpp"
#include "FunctionDef.hpp"
#include "MainThreadTaskQueue.hpp"
#include "NelsonException.hpp"
#include "Scope.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlowGateway {
    //=============================================================================
    namespace {
        //=============================================================================
        constexpr int BRIDGE_TIMEOUT_MS = 30000;
        const char* TIMEOUT_MESSAGE
            = "the interpreter thread did not respond within 30 seconds (busy or blocked)";
        //=============================================================================
        Evaluator* s_eval = nullptr;
        std::thread::id s_evalThreadId;
        bool s_installed = false;
        std::mutex s_installMutex;
        //=============================================================================
        bool
        runViaInterpreter(const std::function<void(Evaluator*)>& work)
        {
            if (std::this_thread::get_id() == s_evalThreadId) {
                work(s_eval);
                return true;
            }
            return MainThreadTaskQueue::getInstance()->runNowIfIdleOrQueue(
                [&work](Evaluator* eval) { work(eval != nullptr ? eval : s_eval); },
                BRIDGE_TIMEOUT_MS);
        }
        //=============================================================================
        bool
        isBareIdentifier(const std::string& text)
        {
            if (text.empty() || (!std::isalpha((unsigned char)text[0]) && text[0] != '_')) {
                return false;
            }
            return std::all_of(text.begin(), text.end(),
                [](unsigned char c) { return std::isalnum(c) || c == '_'; });
        }
        //=============================================================================
        std::string
        trimmed(const std::string& text)
        {
            size_t b = text.find_first_not_of(" \t\r\n");
            if (b == std::string::npos) {
                return {};
            }
            size_t e = text.find_last_not_of(" \t\r\n");
            return text.substr(b, e - b + 1);
        }
        //=============================================================================
        ArrayOf
        doubleColumn(const std::vector<double>& values)
        {
            double* data
                = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
            std::copy(values.begin(), values.end(), data);
            return ArrayOf(NLS_DOUBLE, Dimensions(values.size(), 1), data);
        }
        //=============================================================================
        ArrayOf
        complexColumn(const std::vector<double>& re, const std::vector<double>& im)
        {
            auto* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DCOMPLEX, re.size()));
            for (size_t k = 0; k < re.size(); ++k) {
                data[2 * k] = re[k];
                data[2 * k + 1] = (k < im.size()) ? im[k] : 0.0;
            }
            return ArrayOf(NLS_DCOMPLEX, Dimensions(re.size(), 1), data);
        }
        //=============================================================================
        ArrayOfVector
        fevalCall(Evaluator* eval, const ArrayOf& fcn, const ArrayOfVector& args, int nLhs)
        {
            if (eval == nullptr || eval->getContext() == nullptr) {
                Error(L"Nelson:nflow_engine:evaluatorUnavailable",
                    L"Function evaluator is not available.");
            }
            Context* context = eval->getContext();
            // Call the FunctionDef directly instead of going through the feval builtin,
            // whose argument-marshalling and call-frame layer is wasted on this hot path.
            // Resolution follows feval: a function handle, otherwise a name lookup.
            FunctionDef* funcDef = nullptr;
            if (fcn.isFunctionHandle()) {
                function_handle fh = fcn.asFunctionHandle();
                if (fh.anonymousHandle != nullptr) {
                    funcDef = (FunctionDef*)fh.anonymousHandle;
                }
            } else {
                context->lookupFunction(fcn.asUtf8String(), funcDef);
            }
            if (funcDef == nullptr) {
                Error(L"Nelson:nflow_engine:evaluatorUnavailable",
                    L"Function evaluator is not available.");
            }
            return funcDef->evaluateFunction(eval, args, nLhs);
        }
        //=============================================================================
        NFlow::NelsonCallResult
        doCallFunction(Evaluator* eval, const std::string& fcnSpec, const std::vector<double>& u,
            const std::vector<double>* uImag)
        {
            NFlow::NelsonCallResult result;
            try {
                std::string spec = trimmed(fcnSpec);
                if (spec.empty()) {
                    result.errorMessage = "empty function specification";
                    return result;
                }
                ArrayOf fcnArg;
                if (isBareIdentifier(spec)) {
                    fcnArg = ArrayOf::characterArray(spec);
                } else {
                    // expression using u, or anonymous function source
                    std::string anonymous = (spec[0] == '@') ? spec : ("@(u) (" + spec + ")");
                    FunctionDef* str2func = nullptr;
                    if (eval == nullptr || eval->getContext() == nullptr
                        || !eval->getContext()->lookupFunction("str2func", str2func)
                        || str2func == nullptr) {
                        result.errorMessage = "str2func is not available";
                        return result;
                    }
                    ArrayOfVector in;
                    in << ArrayOf::characterArray(anonymous);
                    ArrayOfVector out = str2func->evaluateFunction(eval, in, 1);
                    if (out.empty()) {
                        result.errorMessage = "cannot build function handle from: " + spec;
                        return result;
                    }
                    fcnArg = out[0];
                }
                ArrayOfVector args;
                args << ((uImag && !uImag->empty()) ? complexColumn(u, *uImag) : doubleColumn(u));
                ArrayOfVector res = fevalCall(eval, fcnArg, args, 1);
                if (res.empty()) {
                    result.errorMessage = "function '" + spec + "' returned no output";
                    return result;
                }
                ArrayOf y = res[0];
                if (!(y.isNumeric() || y.isLogical()) || y.isSparse()) {
                    result.errorMessage = "function '" + spec + "' must return a numeric result";
                    return result;
                }
                if (y.isComplex()) {
                    y.promoteType(NLS_DCOMPLEX);
                    const auto* data = static_cast<const double*>(y.dataPointer());
                    const size_t n = (size_t)y.elementCount();
                    result.y.resize(n);
                    result.yImag.resize(n);
                    for (size_t k = 0; k < n; ++k) {
                        result.y[k] = data[2 * k];
                        result.yImag[k] = data[2 * k + 1];
                    }
                    result.isComplex = true;
                } else {
                    y.promoteType(NLS_DOUBLE);
                    const auto* data = static_cast<const double*>(y.dataPointer());
                    result.y.assign(data, data + y.elementCount());
                }
                result.ok = true;
            } catch (NelsonException& e) {
                result.errorMessage = wstring_to_utf8(e.what());
            } catch (const std::exception& e) {
                result.errorMessage = e.what();
            }
            return result;
        }
        //=============================================================================
        bool
        extractDoubleMatrix(
            ArrayOf value, std::vector<double>& flat, indexType& rows, indexType& cols)
        {
            if (!(value.isNumeric() || value.isLogical()) || value.isComplex() || value.isSparse()
                || !value.is2D()) {
                return false;
            }
            value.promoteType(NLS_DOUBLE);
            rows = value.rows();
            cols = value.columns();
            const auto* data = static_cast<const double*>(value.dataPointer());
            flat.assign(data, data + value.elementCount());
            return true;
        }
        //=============================================================================
        // Like extractDoubleMatrix but complex-capable: flatImag receives the
        // imaginary parts (empty for a real matrix).
        bool
        extractMaybeComplexMatrix(ArrayOf value, std::vector<double>& flat,
            std::vector<double>& flatImag, indexType& rows, indexType& cols, bool& isComplex)
        {
            isComplex = false;
            flatImag.clear();
            if (!(value.isNumeric() || value.isLogical()) || value.isSparse() || !value.is2D()) {
                return false;
            }
            if (value.isComplex()) {
                value.promoteType(NLS_DCOMPLEX);
                rows = value.rows();
                cols = value.columns();
                const auto* data = static_cast<const double*>(value.dataPointer());
                const size_t n = (size_t)value.elementCount();
                flat.resize(n);
                flatImag.resize(n);
                for (size_t k = 0; k < n; ++k) {
                    flat[k] = data[2 * k];
                    flatImag[k] = data[2 * k + 1];
                }
                isComplex = true;
                return true;
            }
            return extractDoubleMatrix(value, flat, rows, cols);
        }
        //=============================================================================
        NFlow::WorkspaceSignal
        doReadSignal(Evaluator* eval, const std::string& variableName)
        {
            NFlow::WorkspaceSignal signal;
            try {
                if (eval == nullptr || eval->getContext() == nullptr) {
                    signal.errorMessage = "evaluator is not initialized";
                    return signal;
                }
                ArrayOf var;
                if (!eval->getContext()->getBaseScope()->lookupVariable(variableName, var)) {
                    signal.errorMessage
                        = "variable '" + variableName + "' not found in base workspace";
                    return signal;
                }
                std::vector<double> timesFlat;
                std::vector<double> valuesFlat;
                indexType nSamples = 0;
                indexType width = 0;
                if (var.isStruct() && var.isScalar()) {
                    if (!var.isField("time") || !var.isField("signals")) {
                        signal.errorMessage = "variable '" + variableName
                            + "' must have 'time' and 'signals' fields";
                        return signal;
                    }
                    indexType tRows = 0;
                    indexType tCols = 0;
                    if (!extractDoubleMatrix(var.field("time"), timesFlat, tRows, tCols)
                        || (tCols > 1 && tRows > 1)) {
                        signal.errorMessage
                            = "variable '" + variableName + "': 'time' must be a double vector";
                        return signal;
                    }
                    ArrayOf signalsField = var.field("signals");
                    if (!signalsField.isStruct() || !signalsField.isScalar()
                        || !signalsField.isField("values")) {
                        signal.errorMessage = "variable '" + variableName
                            + "': 'signals' must be a scalar struct with a 'values' field";
                        return signal;
                    }
                    indexType vRows = 0;
                    std::vector<double> valuesImagFlat;
                    bool valuesComplex = false;
                    if (!extractMaybeComplexMatrix(signalsField.field("values"), valuesFlat,
                            valuesImagFlat, vRows, width, valuesComplex)) {
                        signal.errorMessage = "variable '" + variableName
                            + "': 'signals.values' must be a numeric matrix";
                        return signal;
                    }
                    signal.isComplex = valuesComplex;
                    if (valuesComplex) {
                        // transposed below alongside the real parts
                        signal.valuesImag = std::move(valuesImagFlat);
                    }
                    nSamples = static_cast<indexType>(timesFlat.size());
                    if (vRows != nSamples) {
                        signal.errorMessage = "variable '" + variableName
                            + "': 'signals.values' must have one row per time value";
                        return signal;
                    }
                } else {
                    indexType rows = 0;
                    indexType cols = 0;
                    if (!extractDoubleMatrix(var, valuesFlat, rows, cols) || cols < 2) {
                        signal.errorMessage = "variable '" + variableName
                            + "' must be a [time, values] matrix with at least two columns, or a "
                              "struct with 'time' and 'signals' fields";
                        return signal;
                    }
                    nSamples = rows;
                    width = cols - 1;
                    timesFlat.assign(valuesFlat.begin(), valuesFlat.begin() + rows);
                    valuesFlat.erase(valuesFlat.begin(), valuesFlat.begin() + rows);
                }
                if (nSamples < 1) {
                    signal.errorMessage = "variable '" + variableName + "' has no samples";
                    return signal;
                }
                for (indexType k = 0; k < nSamples; ++k) {
                    if (!std::isfinite(timesFlat[k])) {
                        signal.errorMessage = "variable '" + variableName
                            + "': time values must not contain Inf or NaN";
                        return signal;
                    }
                    if (k > 0 && timesFlat[k] < timesFlat[k - 1]) {
                        signal.errorMessage
                            = "variable '" + variableName + "': time values must be non-decreasing";
                        return signal;
                    }
                }
                signal.times = std::move(timesFlat);
                signal.width = static_cast<int>(width);
                // transpose column-major (rows=samples) to per-sample layout
                signal.values.resize(static_cast<size_t>(nSamples) * width);
                for (indexType k = 0; k < nSamples; ++k) {
                    for (indexType i = 0; i < width; ++i) {
                        signal.values[k * width + i] = valuesFlat[i * nSamples + k];
                    }
                }
                if (signal.isComplex) {
                    std::vector<double> imagFlat = std::move(signal.valuesImag);
                    signal.valuesImag.assign(static_cast<size_t>(nSamples) * width, 0.0);
                    for (indexType k = 0; k < nSamples; ++k) {
                        for (indexType i = 0; i < width; ++i) {
                            const size_t s = (size_t)(i * nSamples + k);
                            if (s < imagFlat.size()) {
                                signal.valuesImag[k * width + i] = imagFlat[s];
                            }
                        }
                    }
                }
                signal.ok = true;
            } catch (NelsonException& e) {
                signal.errorMessage = wstring_to_utf8(e.what());
            } catch (const std::exception& e) {
                signal.errorMessage = e.what();
            }
            return signal;
        }
        //=============================================================================
        ArrayOf
        samplesMatrix(const NFlow::WorkspacePayload& payload)
        {
            const indexType n = static_cast<indexType>(payload.times.size());
            const indexType w = static_cast<indexType>(std::max(payload.width, 1));
            // Typed signals keep their exactness: complex samples become a
            // complex matrix, exact-64 samples become an exact integer matrix.
            if (payload.isComplex) {
                auto* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DCOMPLEX, n * w));
                for (indexType k = 0; k < n; ++k) {
                    for (indexType i = 0; i < w; ++i) {
                        const size_t s = (size_t)(k * w + i);
                        data[2 * (i * n + k)] = payload.values[s];
                        data[2 * (i * n + k) + 1]
                            = (s < payload.valuesImag.size()) ? payload.valuesImag[s] : 0.0;
                    }
                }
                return ArrayOf(NLS_DCOMPLEX, Dimensions(n, w), data);
            }
            if (payload.exactType == "int64" && !payload.valuesI64.empty()) {
                auto* data = static_cast<int64*>(ArrayOf::allocateArrayOf(NLS_INT64, n * w));
                for (indexType k = 0; k < n; ++k) {
                    for (indexType i = 0; i < w; ++i) {
                        data[i * n + k] = (int64)payload.valuesI64[(size_t)(k * w + i)];
                    }
                }
                return ArrayOf(NLS_INT64, Dimensions(n, w), data);
            }
            if (payload.exactType == "uint64" && !payload.valuesI64.empty()) {
                auto* data = static_cast<uint64*>(ArrayOf::allocateArrayOf(NLS_UINT64, n * w));
                for (indexType k = 0; k < n; ++k) {
                    for (indexType i = 0; i < w; ++i) {
                        data[i * n + k]
                            = (uint64)(unsigned long long)payload.valuesI64[(size_t)(k * w + i)];
                    }
                }
                return ArrayOf(NLS_UINT64, Dimensions(n, w), data);
            }
            auto* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n * w));
            for (indexType k = 0; k < n; ++k) {
                for (indexType i = 0; i < w; ++i) {
                    data[i * n + k] = payload.values[k * w + i];
                }
            }
            return ArrayOf(NLS_DOUBLE, Dimensions(n, w), data);
        }
        //=============================================================================
        bool
        doWriteVariable(
            Evaluator* eval, const NFlow::WorkspacePayload& payload, std::string& errorMessage)
        {
            try {
                if (eval == nullptr || eval->getContext() == nullptr) {
                    errorMessage = "evaluator is not initialized";
                    return false;
                }
                if (!isBareIdentifier(payload.variableName)) {
                    errorMessage = "invalid variable name: '" + payload.variableName + "'";
                    return false;
                }
                ArrayOf value;
                if (payload.saveFormat == "Array") {
                    value = samplesMatrix(payload);
                } else {
                    ArrayOf timeValue;
                    if (payload.saveFormat == "Structure") {
                        timeValue = ArrayOf::doubleMatrix2d(0, 0);
                    } else { // "Structure With Time"
                        timeValue = doubleColumn(payload.times);
                    }
                    ArrayOfVector signalsFieldValues;
                    signalsFieldValues << samplesMatrix(payload);
                    signalsFieldValues << ArrayOf::doubleScalar(static_cast<double>(payload.width));
                    signalsFieldValues << ArrayOf::characterArray(std::string(""));
                    ArrayOf signalsValue = ArrayOf::structScalar(
                        stringVector { "values", "dimensions", "label" }, signalsFieldValues);
                    ArrayOfVector topFieldValues;
                    topFieldValues << timeValue;
                    topFieldValues << signalsValue;
                    topFieldValues << ArrayOf::characterArray(payload.blockName);
                    value = ArrayOf::structScalar(
                        stringVector { "time", "signals", "blockName" }, topFieldValues);
                }
                Scope* scope = eval->getContext()->getBaseScope();
                if (scope->isLockedVariable(payload.variableName)) {
                    errorMessage
                        = "variable '" + payload.variableName + "' is locked in base workspace";
                    return false;
                }
                scope->insertVariable(payload.variableName, value);
                return true;
            } catch (NelsonException& e) {
                errorMessage = wstring_to_utf8(e.what());
            } catch (const std::exception& e) {
                errorMessage = e.what();
            }
            return false;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    void
    ensureNelsonWorkspaceBridge(Evaluator* eval)
    {
        if (eval == nullptr) {
            return;
        }
        std::lock_guard<std::mutex> lock(s_installMutex);
        if (s_installed && s_eval == eval) {
            return;
        }
        s_eval = eval;
        s_evalThreadId = std::this_thread::get_id();
        s_installed = true;

        NFlow::NelsonCallFn call
            = [](const std::string& fcnSpec, const std::vector<double>& u,
                  const std::vector<double>* uImag) -> NFlow::NelsonCallResult {
            NFlow::NelsonCallResult result;
            bool delivered = runViaInterpreter([&result, &fcnSpec, &u, uImag](Evaluator* e) {
                result = doCallFunction(e, fcnSpec, u, uImag);
            });
            if (!delivered) {
                result = NFlow::NelsonCallResult {};
                result.errorMessage = TIMEOUT_MESSAGE;
            }
            return result;
        };
        NFlow::WorkspaceReadFn read
            = [](const std::string& variableName) -> NFlow::WorkspaceSignal {
            NFlow::WorkspaceSignal signal;
            bool delivered = runViaInterpreter(
                [&signal, &variableName](Evaluator* e) { signal = doReadSignal(e, variableName); });
            if (!delivered) {
                signal = NFlow::WorkspaceSignal {};
                signal.errorMessage = TIMEOUT_MESSAGE;
            }
            return signal;
        };
        NFlow::WorkspaceWriteFn write
            = [](const NFlow::WorkspacePayload& payload, std::string& errorMessage) -> bool {
            bool ok = false;
            bool delivered = runViaInterpreter([&ok, &payload, &errorMessage](Evaluator* e) {
                ok = doWriteVariable(e, payload, errorMessage);
            });
            if (!delivered) {
                errorMessage = TIMEOUT_MESSAGE;
                return false;
            }
            return ok;
        };
        NFlow::setNelsonWorkspaceBridge(call, read, write);
    }
    //=============================================================================
    std::string
    nflowSspImportViaInterpreter(
        const std::string& sspPath, const std::string& destinationDir, std::string& errorMessage)
    {
        errorMessage.clear();
        std::string nflowPath;
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                args << ArrayOf::characterArray(sspPath);
                if (!destinationDir.empty()) {
                    args << ArrayOf::characterArray(destinationDir);
                }
                ArrayOfVector out
                    = fevalCall(e, ArrayOf::characterArray("NFlow.internal.sspImport"), args, 1);
                if (!out.empty() && out[0].isRowVectorCharacterArray()) {
                    nflowPath = out[0].asUtf8String();
                } else if (!out.empty() && out[0].isScalarStringArray()) {
                    nflowPath = out[0].asUtf8String();
                } else {
                    errorMessage = "SSP import returned no model path.";
                }
            } catch (NelsonException& ex) {
                errorMessage = wstring_to_utf8(ex.what());
            } catch (const std::exception& ex) {
                errorMessage = ex.what();
            } catch (...) {
                errorMessage = "SSP import failed with an unknown error.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        return nflowPath;
    }
    //=============================================================================
    std::string
    nflowSspExportDiagramViaInterpreter(
        const std::string& nflowContent, const std::string& sspPath, std::string& errorMessage)
    {
        errorMessage.clear();
        std::string produced;
        // Stages the diagram content to a temp .nflow and packages it (the inverse of
        // NFlow.sspImport).
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                args << ArrayOf::characterArray(nflowContent);
                args << ArrayOf::characterArray(sspPath);
                ArrayOfVector out = fevalCall(
                    e, ArrayOf::characterArray("NFlow.internal.sspExportContent"), args, 1);
                if (!out.empty()
                    && (out[0].isRowVectorCharacterArray() || out[0].isScalarStringArray())) {
                    produced = out[0].asUtf8String();
                } else {
                    errorMessage = "SSP export returned no archive path.";
                }
            } catch (NelsonException& ex) {
                errorMessage = wstring_to_utf8(ex.what());
            } catch (const std::exception& ex) {
                errorMessage = ex.what();
            } catch (...) {
                errorMessage = "SSP export failed with an unknown error.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        return produced;
    }
    //=============================================================================
    std::string
    nflowExportFmuViaInterpreter(
        const std::string& nflowContent, const std::string& fmuPath, std::string& errorMessage)
    {
        errorMessage.clear();
        std::string produced;
        // Stages the diagram to a temp .nflow, exports an FMI 3.0 CS FMU and moves it
        // to the chosen path.
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                args << ArrayOf::characterArray(nflowContent);
                args << ArrayOf::characterArray(fmuPath);
                ArrayOfVector out = fevalCall(
                    e, ArrayOf::characterArray("NFlow.internal.exportFmuContent"), args, 1);
                if (!out.empty()
                    && (out[0].isRowVectorCharacterArray() || out[0].isScalarStringArray())) {
                    produced = out[0].asUtf8String();
                } else {
                    errorMessage = "FMU export returned no archive path.";
                }
            } catch (NelsonException& ex) {
                errorMessage = wstring_to_utf8(ex.what());
            } catch (const std::exception& ex) {
                errorMessage = ex.what();
            } catch (...) {
                errorMessage = "FMU export failed with an unknown error.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        return produced;
    }
    //=============================================================================
    std::string
    nflowModelicaInfoViaInterpreter(std::string& errorMessage)
    {
        errorMessage.clear();
        std::string infoJson;
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                ArrayOfVector out = fevalCall(
                    e, ArrayOf::characterArray("NFlow.internal.modelicaInfoJson"), args, 1);
                if (!out.empty()
                    && (out[0].isRowVectorCharacterArray() || out[0].isScalarStringArray())) {
                    infoJson = out[0].asUtf8String();
                } else {
                    errorMessage = "OpenModelica detection returned no information.";
                }
            } catch (NelsonException& ex) {
                errorMessage = wstring_to_utf8(ex.what());
            } catch (const std::exception& ex) {
                errorMessage = ex.what();
            } catch (...) {
                errorMessage = "OpenModelica detection failed with an unknown error.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        return infoJson;
    }
    //=============================================================================
    bool
    nflowEvaluateCallbackViaInterpreter(
        const std::string& code, const std::string& blockId, std::string& errorMessage)
    {
        errorMessage.clear();
        if (trimmed(code).empty()) {
            return true;
        }
        bool evaluated = false;
        const bool delivered = runViaInterpreter([&](Evaluator* evaluator) {
            try {
                evaluated = evaluator != nullptr && evaluator->evaluateString(code, true);
                if (!evaluated) {
                    errorMessage = "Callback evaluation failed for block '" + blockId + "'.";
                }
            } catch (NelsonException& exception) {
                errorMessage = wstring_to_utf8(exception.what());
            } catch (const std::exception& exception) {
                errorMessage = exception.what();
            } catch (...) {
                errorMessage = "Callback evaluation failed for block '" + blockId + "'.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        return delivered && evaluated && errorMessage.empty();
    }
    //=============================================================================
    bool
    nflowOpenHelpViaInterpreter(
        const std::string& topic, const std::string& page, std::string& errorMessage)
    {
        errorMessage.clear();
        bool opened = false;
        const bool delivered = runViaInterpreter([&](Evaluator* evaluator) {
            bool pageConfigured = false;
            std::string previousPage;
            auto restorePage = [&]() {
                if (!pageConfigured) {
                    return;
                }
                try {
                    ArrayOfVector restoreArgs;
                    restoreArgs << ArrayOf::characterArray("NELSON_DOC_BLOCK_PAGE")
                                << ArrayOf::characterArray(previousPage);
                    fevalCall(evaluator, ArrayOf::characterArray("setenv"), restoreArgs, 0);
                } catch (...) {
                    // The documentation result is already known; do not hide
                    // it behind a best-effort environment cleanup failure.
                }
            };
            try {
                if (!page.empty()) {
                    ArrayOfVector previousArgs;
                    previousArgs << ArrayOf::characterArray("NELSON_DOC_BLOCK_PAGE");
                    ArrayOfVector previous
                        = fevalCall(evaluator, ArrayOf::characterArray("getenv"), previousArgs, 1);
                    if (!previous.empty()
                        && (previous[0].isRowVectorCharacterArray()
                            || previous[0].isScalarStringArray())) {
                        previousPage = previous[0].asUtf8String();
                    }
                    ArrayOfVector pageArgs;
                    pageArgs << ArrayOf::characterArray("NELSON_DOC_BLOCK_PAGE")
                             << ArrayOf::characterArray(page);
                    fevalCall(evaluator, ArrayOf::characterArray("setenv"), pageArgs, 0);
                    pageConfigured = true;
                }
                ArrayOfVector args;
                args << ArrayOf::characterArray(topic);
                fevalCall(evaluator, ArrayOf::characterArray("doc"), args, 0);
                opened = true;
            } catch (NelsonException& exception) {
                errorMessage = wstring_to_utf8(exception.what());
            } catch (const std::exception& exception) {
                errorMessage = exception.what();
            } catch (...) {
                errorMessage = "Opening documentation failed.";
            }
            restorePage();
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        return delivered && opened && errorMessage.empty();
    }
    //=============================================================================
    std::string
    nflowExpandModelicaViaInterpreter(const std::string& diagramJson, std::string& errorMessage)
    {
        errorMessage.clear();
        // Fast path: a model without a `modelica` block is returned unchanged. The .m
        // expander checks too, but this keeps the common case off the interpreter thread.
        if (diagramJson.find("modelica") == std::string::npos) {
            return diagramJson;
        }
        // The interpreter side compiles each modelica block's model to an FMU
        // (OpenModelica) and rewrites the block to type "fmu", so the engine, which
        // knows nothing about Modelica, simulates a plain FMU diagram. A build failure
        // raises here and blocks the simulation.
        std::string expanded;
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                args << ArrayOf::characterArray(diagramJson);
                ArrayOfVector out = fevalCall(
                    e, ArrayOf::characterArray("NFlow.internal.expandModelicaDoc"), args, 1);
                if (!out.empty()
                    && (out[0].isRowVectorCharacterArray() || out[0].isScalarStringArray())) {
                    expanded = out[0].asUtf8String();
                } else {
                    errorMessage = "Modelica expansion returned no document.";
                }
            } catch (NelsonException& ex) {
                errorMessage = wstring_to_utf8(ex.what());
            } catch (const std::exception& ex) {
                errorMessage = ex.what();
            } catch (...) {
                errorMessage = "Modelica expansion failed with an unknown error.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        if (!errorMessage.empty()) {
            return std::string();
        }
        return expanded;
    }
    //=============================================================================
    bool
    nflowRunStopFcnViaInterpreter(
        const std::string& documentJson, const std::string& resultJson, std::string& errorMessage)
    {
        errorMessage.clear();
        // Fast path: a model without StopFcn needs no thread hop. The .m helper checks
        // too, but this keeps the common case off the interpreter thread.
        if (documentJson.find("stopFcn") == std::string::npos) {
            return false;
        }
        // The interpreter side rebuilds the `out` that sim() exposes and runs the model's
        // StopFcn in the base workspace, so the editor's Run button fires the callback
        // like the command-line sim().
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                args << ArrayOf::characterArray(documentJson);
                args << ArrayOf::characterArray(resultJson);
                fevalCall(e, ArrayOf::characterArray("NFlow.internal.runStopFcnForRun"), args, 0);
            } catch (NelsonException& ex) {
                errorMessage = wstring_to_utf8(ex.what());
            } catch (const std::exception& ex) {
                errorMessage = ex.what();
            } catch (...) {
                errorMessage = "StopFcn execution failed with an unknown error.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        return delivered && errorMessage.empty();
    }
    //=============================================================================
    std::string
    nflowExpandAcausalViaInterpreter(const std::string& diagramJson, std::string& errorMessage)
    {
        errorMessage.clear();
        // Fast path: only diagrams that wire components over undirected physical pins
        // carry `physicalWires`; any other model (causal blocks, or already-lowered
        // island blocks) is returned unchanged.
        if (diagramJson.find("physicalWires") == std::string::npos) {
            return diagramJson;
        }
        // The interpreter side lowers the electrical/thermal/translational/rotational nets
        // (NFlow.internal.acausalExpand) and the planar frames (NFlow.internal.planarExpand)
        // into the engine's native island blocks (physicalIsland,
        // mechanicalTranslationalIsland, planarMechanicalIsland). The engine knows nothing
        // about catalog blocks like Resistor and simulates a plain island diagram. A lowering
        // error raises here and blocks the simulation.
        std::string expanded;
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                args << ArrayOf::characterArray(diagramJson);
                ArrayOfVector out = fevalCall(
                    e, ArrayOf::characterArray("NFlow.internal.expandAcausalDoc"), args, 1);
                if (!out.empty()
                    && (out[0].isRowVectorCharacterArray() || out[0].isScalarStringArray())) {
                    expanded = out[0].asUtf8String();
                } else {
                    errorMessage = "Acausal expansion returned no document.";
                }
            } catch (NelsonException& ex) {
                errorMessage = wstring_to_utf8(ex.what());
            } catch (const std::exception& ex) {
                errorMessage = ex.what();
            } catch (...) {
                errorMessage = "Acausal expansion failed with an unknown error.";
            }
        });
        if (!delivered && errorMessage.empty()) {
            errorMessage = TIMEOUT_MESSAGE;
        }
        if (!errorMessage.empty()) {
            return std::string();
        }
        return expanded;
    }
    //=============================================================================
    std::string
    nflowReduceLinearIslandsViaInterpreter(
        const std::string& diagramJson, std::string& errorMessage)
    {
        errorMessage.clear();
        // Fast path: only already-expanded acausal diagrams carry physicalIsland blocks.
        if (diagramJson.find("physicalIsland") == std::string::npos) {
            return diagramJson;
        }
        std::string reduced;
        bool delivered = runViaInterpreter([&](Evaluator* e) {
            try {
                ArrayOfVector args;
                args << ArrayOf::characterArray(diagramJson);
                ArrayOfVector out = fevalCall(
                    e, ArrayOf::characterArray("NFlow.internal.reduceLinearIslands"), args, 1);
                if (!out.empty()
                    && (out[0].isRowVectorCharacterArray() || out[0].isScalarStringArray())) {
                    reduced = out[0].asUtf8String();
                }
            } catch (...) {
                // Opportunistic pass: a failed reduction (e.g. a singular algebraic
                // block) keeps the raw diagram; the native island lowerings accept
                // it or the non-generatable gate rejects it downstream.
            }
        });
        if (!delivered || reduced.empty()) {
            return diagramJson;
        }
        return reduced;
    }
    //=============================================================================
    nflow::CodegenPipelineHooks
    makeInterpreterCodegenHooks()
    {
        nflow::CodegenPipelineHooks hooks;
        hooks.resolveWorkspaceInputs = [](std::string& diagramJson, std::string& err) {
            return NFlow::resolveWorkspaceInputs(diagramJson, err);
        };
        hooks.expandModelica = [](const std::string& diagramJson, std::string& err) {
            return nflowExpandModelicaViaInterpreter(diagramJson, err);
        };
        hooks.expandAcausal = [](const std::string& diagramJson, std::string& err) {
            return nflowExpandAcausalViaInterpreter(diagramJson, err);
        };
        hooks.reduceLinearIslands = [](const std::string& diagramJson, std::string& err) {
            return nflowReduceLinearIslandsViaInterpreter(diagramJson, err);
        };
        return hooks;
    }
    //=============================================================================
    void
    clearNelsonWorkspaceBridge()
    {
        std::lock_guard<std::mutex> lock(s_installMutex);
        NFlow::setNelsonWorkspaceBridge({}, {}, {});
        s_eval = nullptr;
        s_evalThreadId = std::thread::id {};
        s_installed = false;
    }
    //=============================================================================
} // namespace NFlowGateway
//=============================================================================
} // namespace Nelson
//=============================================================================
