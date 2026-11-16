//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "process_dispatchBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ProcessObject.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
#include "processBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
textScalar(const ArrayOf& value);
static std::string
toBytes(const ArrayOf& value);
static ArrayOfVector
openProcess(const ArrayOfVector& argIn);
static ArrayOfVector
deleteProcess(const ArrayOfVector& argIn);
static ArrayOfVector
isvalidProcess(const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::process_dispatchBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    std::wstring action = textScalar(argIn[0]);
    if (action == L"open") {
        return openProcess(argIn);
    }
    nargincheck(argIn, 2);
    if (action == L"delete") {
        return deleteProcess(argIn);
    }
    if (action == L"isvalid") {
        return isvalidProcess(argIn);
    }
    ProcessObject* object = getProcessObjectScalar(argIn[1]);
    if (action == L"write") {
        nargincheck(argIn, 3, 3);
        std::wstring errorMessage;
        if (!object->writeBytes(toBytes(argIn[2]), errorMessage)) {
            Error(L"Nelson:os_functions:processWriteFailed", errorMessage);
        }
        return {};
    }
    if (action == L"read") {
        return ArrayOfVector { ArrayOf::characterArray(utf8_to_wstring(object->readAvailable())) };
    }
    if (action == L"readline") {
        std::string line;
        if (object->readLine(line)) {
            return ArrayOfVector { ArrayOf::characterArray(utf8_to_wstring(line)) };
        }
        return ArrayOfVector { ArrayOf::characterArray(std::wstring()) };
    }
    if (action == L"stderr") {
        return ArrayOfVector { ArrayOf::characterArray(
            utf8_to_wstring(object->readAvailableError())) };
    }
    if (action == L"close") {
        object->closeInput();
        return {};
    }
    if (action == L"wait") {
        double timeoutSeconds = -1.0;
        if (argIn.size() >= 3) {
            timeoutSeconds = argIn[2].asDoubleScalar();
        }
        object->waitFor(timeoutSeconds);
        return {};
    }
    if (action == L"terminate") {
        object->terminateProcess();
        return {};
    }
    if (action == L"get") {
        nargincheck(argIn, 3, 3);
        std::wstring propertyName = textScalar(argIn[2]);
        ArrayOf res;
        if (!object->get(propertyName, res)) {
            Error(L"Nelson:os_functions:invalidProperty", L"Invalid property: %s.", propertyName);
        }
        return ArrayOfVector { res };
    }
    Error(L"Nelson:os_functions:unknownProcessAction", L"Unknown process action: %s.", action);
    return {};
}
//=============================================================================
static ArrayOfVector
openProcess(const ArrayOfVector& argIn)
{
#if defined(__EMSCRIPTEN__)
    Error(L"Nelson:os_functions:processNotSupported", L"process is not supported in this build.");
#endif
    nargincheck(argIn, 2);
    std::wstring command = textScalar(argIn[1]);
    wstringVector arguments;
    for (size_t k = 2; k < argIn.size(); ++k) {
        arguments.push_back(textScalar(argIn[k]));
    }
    auto* object = new ProcessObject(command, arguments, L"");
    if (!object->started()) {
        std::wstring reason = object->startError();
        delete object;
        Error(L"Nelson:os_functions:processStartFailed", reason);
    }
    return ArrayOfVector { ArrayOf::handle(object) };
}
//=============================================================================
static ArrayOfVector
deleteProcess(const ArrayOfVector& argIn)
{
    if (!argIn[1].isHandle() || argIn[1].handleCategory() != NLS_HANDLE_PROCESS_CATEGORY_STR) {
        Error(L"Nelson:os_functions:processHandleExpected", L"%s expected.", L"process handle");
    }
    std::vector<HandleRef> refs = Nelson::OsFunctionsGateway::getProcessHandleRefs(argIn[1]);
    for (const HandleRef& ref : refs) {
        if (!ref || !ref->isValid()) {
            continue;
        }
        auto* object = dynamic_cast<ProcessObject*>(ref->object());
        if (object != nullptr) {
            object->terminateProcess();
            object->releaseRetainedReference();
            ref->invalidateAs<ProcessObject>(NLS_HANDLE_PROCESS_CATEGORY_STR,
                [](ProcessObject* processObject) { delete processObject; });
        }
    }
    return {};
}
//=============================================================================
static ArrayOfVector
isvalidProcess(const ArrayOfVector& argIn)
{
    bool valid = false;
    if (argIn[1].isHandle() && argIn[1].isScalar()
        && argIn[1].handleCategory() == NLS_HANDLE_PROCESS_CATEGORY_STR) {
        std::vector<HandleRef> refs = Nelson::OsFunctionsGateway::getProcessHandleRefs(argIn[1]);
        valid = !refs.empty() && refs[0] && refs[0]->isValid();
    }
    return ArrayOfVector { ArrayOf::logicalScalar(valid) };
}
//=============================================================================
static std::wstring
textScalar(const ArrayOf& value)
{
    if (!value.isRowVectorCharacterArray() && !value.isScalarStringArray()) {
        Error(L"Nelson:os_functions:textScalarExpected", L"%s expected.",
            L"a character vector or string scalar");
    }
    return value.asWideString();
}
//=============================================================================
static std::string
toBytes(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return wstring_to_utf8(value.asWideString());
    }
    if (value.dataClass() == NLS_UINT8) {
        const auto* data = static_cast<const uint8*>(value.dataPointer());
        return std::string(
            reinterpret_cast<const char*>(data), static_cast<size_t>(value.elementCount()));
    }
    Error(L"Nelson:os_functions:writeDataExpected", L"%s expected.",
        L"a character vector, string scalar, or uint8 vector");
    return {};
}
//=============================================================================
