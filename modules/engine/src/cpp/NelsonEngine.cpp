//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#define _WIN32_WINNT 0x0550
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "NelsonEngine.h"
#include "ArrayOf.hpp"
#include "IpcReadyReceiverNamedMutex.hpp"
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "NelsonReadyNamedMutex.hpp"
#include "SystemCommand.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <Eigen/Sparse>
#include <complex>
#include <cstring>
#include <filesystem>
#include <new>
#include <string>
#include <thread>
#include <tuple>
#include <type_traits>
#include <vector>
#ifdef _MSC_VER
#include <windows.h>
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif
#else
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#endif
//=============================================================================
namespace {
//=============================================================================
constexpr int TIMEOUT_SECONDS = 20;
constexpr wchar_t NELSON_DEFAULT_EXECUTABLE[] = L"nelson-gui";
int activeEngineCount = 0;
//=============================================================================
std::filesystem::path
getNelsonRootFromEnvironment()
{
    const char* root = std::getenv("NELSON_ROOT");
    if (root == nullptr || root[0] == '\0') {
        return {};
    }
    return std::filesystem::path(Nelson::utf8_to_wstring(root));
}
//=============================================================================
std::filesystem::path
resolveNelsonExecutable(const std::wstring& executableName)
{
    std::filesystem::path root = getNelsonRootFromEnvironment();
    if (!root.empty()) {
#ifdef _MSC_VER
        const std::vector<std::filesystem::path> candidates = {
            root / L"bin" / L"x64" / L"NelSon-gui.exe",
            root / L"bin" / L"win32" / L"NelSon-gui.exe",
            root / L"bin" / L"ARM64" / L"NelSon-gui.exe",
            root / L"bin" / L"NelSon-gui.exe",
        };
#else
        const std::vector<std::filesystem::path> candidates = {
            root / "bin" / "linux" / "nelson-gui",
            root / "bin" / "macOS" / "nelson-gui",
            root / "bin" / "nelson-gui",
        };
#endif
        for (const auto& candidate : candidates) {
            if (std::filesystem::exists(candidate)) {
                return candidate;
            }
        }
    }
    return std::filesystem::path(executableName);
}
//=============================================================================
#ifdef _MSC_VER
using PROCESS_PID_T = unsigned long;
struct ProcessChild
{
    ProcessChild() = default;
    explicit ProcessChild(PROCESS_PID_T p) { attach(p); }
    ProcessChild(HANDLE h, PROCESS_PID_T p) : handle(h), pid(p) { }
    ~ProcessChild()
    {
        if (handle) {
            CloseHandle(handle);
        }
    }
    bool
    valid() const
    {
        return pid != 0;
    }
    PROCESS_PID_T
    id() const { return pid; }
    void
    attach(PROCESS_PID_T p)
    {
        pid = p;
        handle = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid);
    }
    HANDLE handle = nullptr;
    PROCESS_PID_T pid = 0;
};
#else
using PROCESS_PID_T = pid_t;
struct ProcessChild
{
    ProcessChild() = default;
    explicit ProcessChild(PROCESS_PID_T p) : pid(p) { }
    bool
    valid() const
    {
        return pid > 0;
    }
    PROCESS_PID_T
    id() const { return pid; }
    PROCESS_PID_T pid = -1;
};
#endif
//=============================================================================
char*
copyString(const std::string& value)
{
    char* result = new (std::nothrow) char[value.size() + 1];
    if (result == nullptr) {
        return nullptr;
    }
    std::memcpy(result, value.c_str(), value.size() + 1);
    return result;
}
//=============================================================================
void
setString(char** target, const std::string& value)
{
    if (target != nullptr) {
        *target = copyString(value);
    }
}
//=============================================================================
void
setWideString(char** target, const std::wstring& value)
{
    setString(target, Nelson::wstring_to_utf8(value));
}
//=============================================================================
ProcessChild*
attachChild(int pid)
{
    try {
        return new ProcessChild((PROCESS_PID_T)pid);
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
}
//=============================================================================
ProcessChild*
startChild(const std::wstring& executableName, const std::wstring& arguments)
{
    std::filesystem::path executablePath = resolveNelsonExecutable(executableName);
    std::filesystem::path workingDirectory = getNelsonRootFromEnvironment();
#ifdef _MSC_VER
    std::wstring cmdline = L"\"" + executablePath.wstring() + L"\"";
    if (!arguments.empty()) {
        cmdline += L" " + arguments;
    }
    STARTUPINFOW si {};
    PROCESS_INFORMATION pi {};
    si.cb = sizeof(si);
    BOOL ok = CreateProcessW(nullptr, &cmdline[0], nullptr, nullptr, FALSE, CREATE_NEW_CONSOLE,
        nullptr, workingDirectory.empty() ? nullptr : workingDirectory.wstring().c_str(), &si, &pi);
    if (!ok) {
        return nullptr;
    }
    CloseHandle(pi.hThread);
    return new ProcessChild(pi.hProcess, (PROCESS_PID_T)pi.dwProcessId);
#else
#if defined(__APPLE__) || defined(__MACH__)
    std::filesystem::path p = executablePath;
    std::wstring command = L"open -a \"" + p.generic_wstring() + L"\" --args " + arguments;
#else
    std::filesystem::path p = executablePath;
    std::wstring command;
    if (!workingDirectory.empty()) {
        command = L"cd \"" + workingDirectory.generic_wstring() + L"\" && ";
    }
    command += p.generic_wstring() + L" " + arguments + L" &";
#endif
    size_t mainEvaluatorID = 0;
    auto res = Nelson::SystemCommand(command, 0, false, mainEvaluatorID);
    if (std::get<0>(res) == -1) {
        return nullptr;
    }
    int latestNelsonPID = 0;
    for (int elapsed = 0; elapsed < TIMEOUT_SECONDS; ++elapsed) {
        latestNelsonPID = Nelson::getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE::GUI);
        if (latestNelsonPID > 0) {
            break;
        }
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
    return latestNelsonPID > 0 ? attachChild(latestNelsonPID) : nullptr;
#endif
}
//=============================================================================
bool
waitUntilNelsonIsReady(int pid)
{
    for (int elapsed = 0; elapsed < TIMEOUT_SECONDS; ++elapsed) {
        if (Nelson::haveIsReadyNelsonMutex(pid)) {
            return true;
        }
        if (!Nelson::isPIDRunning(pid)) {
            return false;
        }
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
    return false;
}
//=============================================================================
bool
waitUntilIpcReceiverIsReady(int pid)
{
    for (int elapsed = 0; elapsed < TIMEOUT_SECONDS; ++elapsed) {
        if (Nelson::haveIpcReceiverIsReadyMutex(pid)) {
            return true;
        }
        if (!Nelson::isPIDRunning(pid)) {
            return false;
        }
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
    return false;
}
//=============================================================================
bool
ensureParentReceiver(std::string& error)
{
    if (activeEngineCount > 0) {
        return true;
    }
    int parentPID = Nelson::getCurrentPID();
    if (!Nelson::createNelsonInterprocessReceiver(parentPID, false)) {
        error = "Cannot create Nelson IPC receiver.";
        return false;
    }
    if (!waitUntilIpcReceiverIsReady(parentPID)) {
        error = "Nelson IPC receiver did not become ready.";
        return false;
    }
    return true;
}
//=============================================================================
std::vector<Nelson::indexType>
dimensionsVector(Nelson::ArrayOf value)
{
    Nelson::Dimensions dims = value.getDimensions();
    return dims.getAsVector();
}
//=============================================================================
template <typename T>
void
appendNumericData(nlohmann::json& data, const Nelson::ArrayOf& value)
{
    const T* values = static_cast<const T*>(value.getDataPointer());
    Nelson::indexType count = value.getElementCount();
    for (Nelson::indexType k = 0; k < count; ++k) {
        data.push_back(values[k]);
    }
}
//=============================================================================
template <typename T>
void
appendComplexData(nlohmann::json& realData, nlohmann::json& imagData, const Nelson::ArrayOf& value)
{
    const std::complex<T>* values = static_cast<const std::complex<T>*>(value.getDataPointer());
    Nelson::indexType count = value.getElementCount();
    for (Nelson::indexType k = 0; k < count; ++k) {
        realData.push_back(values[k].real());
        imagData.push_back(values[k].imag());
    }
}
//=============================================================================
template <typename T>
void
fillNumericData(void* destination, const nlohmann::json& data)
{
    T* values = static_cast<T*>(destination);
    for (size_t k = 0; k < data.size(); ++k) {
        values[k] = data[k].get<T>();
    }
}
//=============================================================================
template <typename T>
void
fillComplexData(void* destination, const nlohmann::json& realData, const nlohmann::json& imagData)
{
    std::complex<T>* values = static_cast<std::complex<T>*>(destination);
    for (size_t k = 0; k < realData.size(); ++k) {
        values[k] = std::complex<T>(realData[k].get<T>(), imagData[k].get<T>());
    }
}
//=============================================================================
template <typename T>
bool
sparseTypeIsComplex()
{
    return false;
}
//=============================================================================
template <>
bool
sparseTypeIsComplex<doublecomplex>()
{
    return true;
}
//=============================================================================
template <typename T>
void
appendSparseJsonValue(
    nlohmann::json& data, nlohmann::json& realData, nlohmann::json& imagData, const T& value)
{
    data.push_back(value);
}
//=============================================================================
template <>
void
appendSparseJsonValue<doublecomplex>(nlohmann::json& data, nlohmann::json& realData,
    nlohmann::json& imagData, const doublecomplex& value)
{
    realData.push_back(value.real());
    imagData.push_back(value.imag());
}
//=============================================================================
template <typename T>
T
sparseJsonValueAt(const nlohmann::json& data, size_t index)
{
    return data[index].get<T>();
}
//=============================================================================
template <>
doublecomplex
sparseJsonValueAt<doublecomplex>(const nlohmann::json& data, size_t index)
{
    return doublecomplex(
        data.at("real")[index].get<double>(), data.at("imag")[index].get<double>());
}
//=============================================================================
template <typename T>
nlohmann::json
sparseToJson(const Nelson::ArrayOf& value, const std::string& className)
{
    nlohmann::json result;
    result["class"] = className;
    result["size"] = dimensionsVector(value);
    result["is_sparse"] = true;
    result["is_complex"] = sparseTypeIsComplex<T>();

    nlohmann::json rows = nlohmann::json::array();
    nlohmann::json cols = nlohmann::json::array();
    nlohmann::json data = nlohmann::json::array();
    nlohmann::json realData = nlohmann::json::array();
    nlohmann::json imagData = nlohmann::json::array();

    const auto* sparse = static_cast<const Eigen::SparseMatrix<T, 0, Nelson::signedIndexType>*>(
        value.getSparseDataPointer());
    for (int outer = 0; outer < sparse->outerSize(); ++outer) {
        for (typename Eigen::SparseMatrix<T, 0, Nelson::signedIndexType>::InnerIterator it(
                 *sparse, outer);
            it; ++it) {
            rows.push_back((Nelson::indexType)it.row());
            cols.push_back((Nelson::indexType)it.col());
            appendSparseJsonValue<T>(data, realData, imagData, it.value());
        }
    }
    result["rows"] = rows;
    result["cols"] = cols;
    if (sparseTypeIsComplex<T>()) {
        result["data"] = { { "real", realData }, { "imag", imagData } };
    } else {
        result["data"] = data;
    }
    return result;
}
//=============================================================================
template <typename T>
void*
sparseFromJson(const nlohmann::json& input)
{
    std::vector<Nelson::indexType> dimsVector
        = input.value("size", std::vector<Nelson::indexType> { 0, 0 });
    Nelson::indexType rows = dimsVector.empty() ? 0 : dimsVector[0];
    Nelson::indexType cols = dimsVector.size() < 2 ? 1 : dimsVector[1];
    auto* sparse = new Eigen::SparseMatrix<T, 0, Nelson::signedIndexType>((int)rows, (int)cols);
    std::vector<Eigen::Triplet<T, Nelson::signedIndexType>> triplets;
    const nlohmann::json& rowValues = input.at("rows");
    const nlohmann::json& colValues = input.at("cols");
    const nlohmann::json& data = input.at("data");
    for (size_t k = 0; k < rowValues.size(); ++k) {
        triplets.emplace_back((Nelson::signedIndexType)rowValues[k].get<Nelson::indexType>(),
            (Nelson::signedIndexType)colValues[k].get<Nelson::indexType>(),
            sparseJsonValueAt<T>(data, k));
    }
    sparse->setFromTriplets(triplets.begin(), triplets.end());
    sparse->makeCompressed();
    return sparse;
}
//=============================================================================
nlohmann::json
arrayToJson(const Nelson::ArrayOf& value)
{
    nlohmann::json result;
    result["size"] = dimensionsVector(value);
    result["class"] = "";
    result["is_complex"] = value.isComplex();
    result["is_sparse"] = value.isSparse();

    if (value.isSparse()) {
        // Direct sparse IPC serialization can dereference invalid sparse storage in
        // GUI-owned sessions. Python reads sparse values through full(...) instead.
        result["class"] = "object";
        result["data"] = nullptr;
        result["compatibility_error"] = "Sparse values use the Python compatibility path.";
        return result;
    }

    if (value.isCharacterArray()) {
        result["class"] = "char";
        result["data"] = Nelson::wstring_to_utf8(value.getContentAsWideString());
        return result;
    }

    if ((value.isTable() && value.getClassType() == "table") || value.isStruct()) {
        // Structs and tables are read field-by-field from Python. Recursing through
        // class/struct storage here has caused intermittent GUI access violations.
        result["class"] = "object";
        result["data"] = nullptr;
        result["compatibility_error"]
            = "Struct and table values use the Python compatibility path.";
        return result;
    }

    if (value.isCell()) {
        result["class"] = "cell";
        result["is_complex"] = false;
        result["is_sparse"] = false;
        nlohmann::json elements = nlohmann::json::array();
        const auto* cellData = static_cast<const Nelson::ArrayOf*>(value.getDataPointer());
        Nelson::indexType count = value.getElementCount();
        for (Nelson::indexType k = 0; k < count; ++k) {
            elements.push_back(arrayToJson(cellData[k]));
        }
        result["data"] = elements;
        return result;
    }

    nlohmann::json data = nlohmann::json::array();
    switch (value.getDataClass()) {
    case Nelson::NLS_DOUBLE:
        result["class"] = "double";
        appendNumericData<double>(data, value);
        break;
    case Nelson::NLS_SINGLE:
        result["class"] = "single";
        appendNumericData<Nelson::single>(data, value);
        break;
    case Nelson::NLS_DCOMPLEX: {
        result["class"] = "double";
        nlohmann::json realData = nlohmann::json::array();
        nlohmann::json imagData = nlohmann::json::array();
        appendComplexData<double>(realData, imagData, value);
        result["data"] = { { "real", realData }, { "imag", imagData } };
        return result;
    }
    case Nelson::NLS_SCOMPLEX: {
        result["class"] = "single";
        nlohmann::json realData = nlohmann::json::array();
        nlohmann::json imagData = nlohmann::json::array();
        appendComplexData<Nelson::single>(realData, imagData, value);
        result["data"] = { { "real", realData }, { "imag", imagData } };
        return result;
    }
    case Nelson::NLS_INT8:
        result["class"] = "int8";
        appendNumericData<Nelson::int8>(data, value);
        break;
    case Nelson::NLS_INT16:
        result["class"] = "int16";
        appendNumericData<Nelson::int16>(data, value);
        break;
    case Nelson::NLS_INT32:
        result["class"] = "int32";
        appendNumericData<Nelson::int32>(data, value);
        break;
    case Nelson::NLS_INT64:
        result["class"] = "int64";
        appendNumericData<Nelson::int64>(data, value);
        break;
    case Nelson::NLS_UINT8:
        result["class"] = "uint8";
        appendNumericData<Nelson::uint8>(data, value);
        break;
    case Nelson::NLS_UINT16:
        result["class"] = "uint16";
        appendNumericData<Nelson::uint16>(data, value);
        break;
    case Nelson::NLS_UINT32:
        result["class"] = "uint32";
        appendNumericData<Nelson::uint32>(data, value);
        break;
    case Nelson::NLS_UINT64:
        result["class"] = "uint64";
        appendNumericData<Nelson::uint64>(data, value);
        break;
    case Nelson::NLS_LOGICAL:
        result["class"] = "logical";
        appendNumericData<Nelson::logical>(data, value);
        break;
    default:
        result["class"] = "object";
        result["data"] = nullptr;
        return result;
    }
    result["data"] = data;
    return result;
}
//=============================================================================
Nelson::ArrayOf
jsonToArray(const nlohmann::json& input)
{
    std::string cls = input.value("class", "double");
    if (cls == "char" || cls == "string") {
        return Nelson::ArrayOf::characterArrayConstructor(input.value("data", std::string()));
    }
    std::vector<Nelson::indexType> dimsVector
        = input.value("size", std::vector<Nelson::indexType> { 1, 1 });
    Nelson::Dimensions dims(dimsVector);
    const nlohmann::json& data = input.at("data");
    if (cls == "cell") {
        std::vector<Nelson::ArrayOf> values;
        for (const auto& item : data) {
            values.push_back(jsonToArray(item));
        }
        Nelson::ArrayOfMatrix matrix;
        Nelson::indexType rows = dimsVector.empty() ? 0 : dimsVector[0];
        Nelson::indexType cols = dimsVector.size() < 2 ? 1 : dimsVector[1];
        for (Nelson::indexType r = 0; r < rows; ++r) {
            Nelson::ArrayOfVector row;
            for (Nelson::indexType c = 0; c < cols; ++c) {
                row.push_back(values[c * rows + r]);
            }
            matrix.push_back(row);
        }
        return Nelson::ArrayOf::cellConstructor(matrix);
    }
    if (cls == "struct") {
        Nelson::stringVector fieldNames = input.value("fields", Nelson::stringVector());
        Nelson::ArrayOfVector values;
        for (const auto& fieldName : fieldNames) {
            values.push_back(jsonToArray(data.at(fieldName)));
        }
        return Nelson::ArrayOf::structScalarConstructor(fieldNames, values);
    }
    if (cls == "table") {
        Nelson::stringVector variableNames = input.value("variables", Nelson::stringVector());
        Nelson::stringVector rowNames = input.value("row_names", Nelson::stringVector());
        Nelson::ArrayOfVector values;
        for (const auto& variableName : variableNames) {
            values.push_back(jsonToArray(data.at(variableName)));
        }
        return Nelson::ArrayOf::tableConstructor(values, variableNames, rowNames);
    }
    bool isSparse = input.value("is_sparse", false);
    bool isComplex = input.value("is_complex", false);
    if (isSparse) {
        if (cls == "logical") {
            return Nelson::ArrayOf(
                Nelson::NLS_LOGICAL, dims, sparseFromJson<Nelson::logical>(input), true);
        }
        if (cls == "double" && isComplex) {
            return Nelson::ArrayOf(
                Nelson::NLS_DCOMPLEX, dims, sparseFromJson<doublecomplex>(input), true);
        }
        if (cls == "double") {
            return Nelson::ArrayOf(Nelson::NLS_DOUBLE, dims, sparseFromJson<double>(input), true);
        }
    }
    Nelson::NelsonType type = Nelson::NLS_DOUBLE;
    if (cls == "single" && isComplex) {
        type = Nelson::NLS_SCOMPLEX;
    } else if (cls == "double" && isComplex) {
        type = Nelson::NLS_DCOMPLEX;
    } else if (cls == "single") {
        type = Nelson::NLS_SINGLE;
    } else if (cls == "int8") {
        type = Nelson::NLS_INT8;
    } else if (cls == "int16") {
        type = Nelson::NLS_INT16;
    } else if (cls == "int32") {
        type = Nelson::NLS_INT32;
    } else if (cls == "int64") {
        type = Nelson::NLS_INT64;
    } else if (cls == "uint8") {
        type = Nelson::NLS_UINT8;
    } else if (cls == "uint16") {
        type = Nelson::NLS_UINT16;
    } else if (cls == "uint32") {
        type = Nelson::NLS_UINT32;
    } else if (cls == "uint64") {
        type = Nelson::NLS_UINT64;
    } else if (cls == "logical") {
        type = Nelson::NLS_LOGICAL;
    }

    void* values = Nelson::ArrayOf::allocateArrayOf(
        type, dims.getElementCount(), Nelson::stringVector(), false);
    switch (type) {
    case Nelson::NLS_DCOMPLEX:
        fillComplexData<double>(values, data.at("real"), data.at("imag"));
        break;
    case Nelson::NLS_SCOMPLEX:
        fillComplexData<Nelson::single>(values, data.at("real"), data.at("imag"));
        break;
    case Nelson::NLS_DOUBLE:
        fillNumericData<double>(values, data);
        break;
    case Nelson::NLS_SINGLE:
        fillNumericData<Nelson::single>(values, data);
        break;
    case Nelson::NLS_INT8:
        fillNumericData<Nelson::int8>(values, data);
        break;
    case Nelson::NLS_INT16:
        fillNumericData<Nelson::int16>(values, data);
        break;
    case Nelson::NLS_INT32:
        fillNumericData<Nelson::int32>(values, data);
        break;
    case Nelson::NLS_INT64:
        fillNumericData<Nelson::int64>(values, data);
        break;
    case Nelson::NLS_UINT8:
        fillNumericData<Nelson::uint8>(values, data);
        break;
    case Nelson::NLS_UINT16:
        fillNumericData<Nelson::uint16>(values, data);
        break;
    case Nelson::NLS_UINT32:
        fillNumericData<Nelson::uint32>(values, data);
        break;
    case Nelson::NLS_UINT64:
        fillNumericData<Nelson::uint64>(values, data);
        break;
    case Nelson::NLS_LOGICAL:
        fillNumericData<Nelson::logical>(values, data);
        break;
    default:
        fillNumericData<double>(values, data);
        break;
    }
    return Nelson::ArrayOf(type, dims, values);
}
//=============================================================================
} // namespace
//=============================================================================
struct nelson_engine_tag
{
    ProcessChild* child = nullptr;
    bool owned = false;
};
//=============================================================================
NelsonEngineHandle*
nelson_engine_start(const char* option, char** errorMessage)
{
    std::string error;
    bool receiverCreated = activeEngineCount == 0;
    if (!ensureParentReceiver(error)) {
        setString(errorMessage, error);
        return nullptr;
    }
    std::wstring arguments
        = option == nullptr || option[0] == '\0' ? L"--minimize" : Nelson::utf8_to_wstring(option);
    ProcessChild* child = startChild(NELSON_DEFAULT_EXECUTABLE, arguments);
    if (child == nullptr || !child->valid()) {
        delete child;
        if (receiverCreated) {
            Nelson::removeNelsonInterprocessReceiver(Nelson::getCurrentPID(), false);
        }
        setString(errorMessage, "Cannot start Nelson engine process.");
        return nullptr;
    }
    int childPID = (int)child->id();
    if (!waitUntilNelsonIsReady(childPID) || !waitUntilIpcReceiverIsReady(childPID)) {
#ifndef _MSC_VER
        kill(child->id(), SIGKILL);
#endif
        delete child;
        if (receiverCreated) {
            Nelson::removeNelsonInterprocessReceiver(Nelson::getCurrentPID(), false);
        }
        setString(errorMessage, "Nelson engine process did not become ready.");
        return nullptr;
    }
    NelsonEngineHandle* engine = new (std::nothrow) NelsonEngineHandle;
    if (engine == nullptr) {
        delete child;
        if (receiverCreated) {
            Nelson::removeNelsonInterprocessReceiver(Nelson::getCurrentPID(), false);
        }
        setString(errorMessage, "Out of memory while creating Nelson engine handle.");
        return nullptr;
    }
    engine->child = child;
    engine->owned = true;
    activeEngineCount++;
    return engine;
}
//=============================================================================
NelsonEngineHandle*
nelson_engine_connect(int pid, char** errorMessage)
{
    std::string error;
    bool receiverCreated = activeEngineCount == 0;
    if (!ensureParentReceiver(error)) {
        setString(errorMessage, error);
        return nullptr;
    }
    if (pid <= 0) {
        pid = Nelson::getLatestPidInSharedMemory();
    }
    ProcessChild* child = attachChild(pid);
    if (child == nullptr || !child->valid()) {
        delete child;
        if (receiverCreated) {
            Nelson::removeNelsonInterprocessReceiver(Nelson::getCurrentPID(), false);
        }
        setString(errorMessage, "Cannot connect to Nelson engine process.");
        return nullptr;
    }
    if (!waitUntilNelsonIsReady(pid) || !waitUntilIpcReceiverIsReady(pid)) {
        delete child;
        if (receiverCreated) {
            Nelson::removeNelsonInterprocessReceiver(Nelson::getCurrentPID(), false);
        }
        setString(errorMessage, "Target Nelson process is not ready.");
        return nullptr;
    }
    NelsonEngineHandle* engine = new (std::nothrow) NelsonEngineHandle;
    if (engine == nullptr) {
        delete child;
        if (receiverCreated) {
            Nelson::removeNelsonInterprocessReceiver(Nelson::getCurrentPID(), false);
        }
        setString(errorMessage, "Out of memory while creating Nelson engine handle.");
        return nullptr;
    }
    engine->child = child;
    engine->owned = false;
    activeEngineCount++;
    return engine;
}
//=============================================================================
int
nelson_engine_find(int* pids, int capacity)
{
    std::vector<int> available = Nelson::getNelsonPIDs();
    if (pids != nullptr && capacity > 0) {
        int count = std::min<int>((int)available.size(), capacity);
        for (int k = 0; k < count; ++k) {
            pids[k] = available[k];
        }
    }
    return (int)available.size();
}
//=============================================================================
int
nelson_engine_eval(
    NelsonEngineHandle* engine, const char* command, char** output, char** errorMessage)
{
    if (engine == nullptr || engine->child == nullptr || !engine->child->valid()
        || command == nullptr) {
        setString(errorMessage, "Invalid Nelson engine handle or command.");
        return 1;
    }
    std::wstring result;
    std::wstring error;
    bool ok = Nelson::evalCommandToNelsonInterprocessReceiver(
        (int)engine->child->id(), Nelson::utf8_to_wstring(command), false, result, error);
    setWideString(output, result);
    if (!ok || !error.empty()) {
        setWideString(errorMessage, error.empty() ? L"Nelson command failed." : error);
        return 1;
    }
    return 0;
}
//=============================================================================
int
nelson_engine_get_variable_json(
    NelsonEngineHandle* engine, const char* name, char** json, char** errorMessage)
{
    if (engine == nullptr || engine->child == nullptr || !engine->child->valid()
        || name == nullptr) {
        setString(errorMessage, "Invalid Nelson engine handle or variable name.");
        return 1;
    }
    std::wstring error;
    Nelson::ArrayOf value = Nelson::getVariableFromNelsonInterprocessReceiver(
        (int)engine->child->id(), Nelson::utf8_to_wstring(name), L"base", false, error);
    if (!error.empty()) {
        setWideString(errorMessage, error);
        return 1;
    }
    setString(json, arrayToJson(value).dump());
    return 0;
}
//=============================================================================
int
nelson_engine_put_variable_json(
    NelsonEngineHandle* engine, const char* name, const char* json, char** errorMessage)
{
    if (engine == nullptr || engine->child == nullptr || !engine->child->valid() || name == nullptr
        || json == nullptr) {
        setString(errorMessage, "Invalid Nelson engine handle, variable name, or JSON value.");
        return 1;
    }
    try {
        Nelson::ArrayOf value = jsonToArray(nlohmann::json::parse(json));
        std::wstring error;
        bool ok = Nelson::sendVariableToNelsonInterprocessReceiver(
            (int)engine->child->id(), value, Nelson::utf8_to_wstring(name), L"base", false, error);
        if (!ok || !error.empty()) {
            setWideString(
                errorMessage, error.empty() ? L"Cannot send variable to Nelson engine." : error);
            return 1;
        }
        return 0;
    } catch (const std::exception& e) {
        setString(errorMessage, e.what());
        return 1;
    }
}
//=============================================================================
int
nelson_engine_set_visible(NelsonEngineHandle* engine, bool visible, char** errorMessage)
{
    if (engine == nullptr || engine->child == nullptr || !engine->child->valid()) {
        setString(errorMessage, "Invalid Nelson engine handle.");
        return 1;
    }
    std::wstring error;
    bool ok = Nelson::sendMinimizeToNelsonInterprocessReceiver(
        (int)engine->child->id(), !visible, false, error);
    if (!ok || !error.empty()) {
        setWideString(errorMessage, error.empty() ? L"Cannot change Nelson visibility." : error);
        return 1;
    }
    return 0;
}
//=============================================================================
int
nelson_engine_get_visible(NelsonEngineHandle* engine, bool* visible, char** errorMessage)
{
    if (visible != nullptr) {
        *visible = false;
    }
    if (engine == nullptr || engine->child == nullptr || !engine->child->valid()) {
        setString(errorMessage, "Invalid Nelson engine handle.");
        return 1;
    }
    std::wstring error;
    bool minimized
        = Nelson::isMinimizedFromNelsonInterprocessReceiver((int)engine->child->id(), false, error);
    if (!error.empty()) {
        setWideString(errorMessage, error);
        return 1;
    }
    if (visible != nullptr) {
        *visible = !minimized;
    }
    return 0;
}
//=============================================================================
int
nelson_engine_close(NelsonEngineHandle* engine, char** errorMessage)
{
    if (engine == nullptr) {
        setString(errorMessage, "Invalid Nelson engine handle.");
        return 1;
    }
    if (engine->owned && engine->child != nullptr && engine->child->valid()) {
        char* output = nullptr;
        char* error = nullptr;
        nelson_engine_eval(engine, "quit;", &output, &error);
        nelson_engine_free_string(output);
        nelson_engine_free_string(error);
    }
    delete engine->child;
    delete engine;
    activeEngineCount = std::max(0, activeEngineCount - 1);
    if (activeEngineCount == 0) {
        Nelson::removeNelsonInterprocessReceiver(Nelson::getCurrentPID(), false);
    }
    return 0;
}
//=============================================================================
void
nelson_engine_free_string(char* value)
{
    delete[] value;
}
//=============================================================================
