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
#include <atomic>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <vector>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsOs_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Opaque per-platform channel holding the native process and pipe handles.
struct ProcessChannel;
//=============================================================================
class NLSOS_FUNCTIONS_IMPEXP ProcessObject : public HandleGenericObject
{
public:
    ProcessObject(const std::wstring& command, const wstringVector& arguments,
        const std::wstring& workingDirectory);
    ~ProcessObject() override;

    void
    onDelete(Evaluator* eval) override;

    // True when the native process was launched successfully.
    bool
    started() const;
    // Reason text set when started() is false.
    std::wstring
    startError() const;

    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    bool
    isProperty(const std::wstring& propertyName) override;
    bool
    isMethod(const std::wstring& methodName) override;
    wstringVector
    properties() override;
    wstringVector
    methods() override;
    bool
    disp(Interface* io);

    // Send bytes to the process standard input.
    bool
    writeBytes(const std::string& bytes, std::wstring& errorMessage);
    // Close the process standard input, signalling end of input.
    void
    closeInput();
    // Drain and return all standard output bytes buffered so far.
    std::string
    readAvailable();
    // Pop one newline-terminated line if present; returns false when none is ready.
    bool
    readLine(std::string& line);
    // Drain and return all standard error bytes buffered so far.
    std::string
    readAvailableError();
    // Terminate the process and its children.
    void
    terminateProcess();
    // Block until the process exits or the timeout elapses; negative waits forever.
    void
    waitFor(double timeoutSeconds);

    bool
    isRunning();
    bool
    hasExited();
    int
    exitCode();
    size_t
    bytesAvailable();
    int64 processId() const;

    void
    retainUntilDeleted();
    void
    releaseRetainedReference();

private:
    std::wstring command;
    wstringVector arguments;
    std::wstring workingDirectory;
    std::atomic<bool> launched;
    std::wstring launchError;

    std::unique_ptr<ProcessChannel> channel;
    std::thread stdoutReader;
    std::thread stderrReader;
    std::mutex outMutex;
    std::string outBuffer;
    std::mutex errMutex;
    std::string errBuffer;
    std::mutex writeMutex;

    wstringVector propertiesNames;
    wstringVector methodsNames;

    void
    launch();
    void
    stdoutReaderLoop();
    void
    stderrReaderLoop();
    void
    joinReaders();
    void
    closeStdin();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
