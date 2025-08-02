//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <stdio.h>
#include "PythonLibraryWrapper.hpp"
#include "PythonEngine.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "PythonEnvironment.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static PyObject* mainPythonInterpreter = nullptr;
//=============================================================================
bool
PyIsInitializedInterpreter(void)
{
    return NLSPy_IsInitialized();
}
//=============================================================================
bool
PyInitializeInterpreter(void)
{
    auto readFileIntoContent = [&](const std::wstring& filename, std::string& content) -> bool {
        std::ifstream inputFile(
#ifdef _MSC_VER
            filename, std::ios::in | std::ios::binary
#else
            wstring_to_utf8(filename), std::ios::in | std::ios::binary
#endif
        );

        if (!inputFile.is_open()) {
            return false;
        }

        inputFile.seekg(0, std::ios::end);
        size_t fileSize = inputFile.tellg();
        inputFile.seekg(0, std::ios::beg);
        content.resize(fileSize);
        inputFile.read(content.data(), fileSize);
        inputFile.close();

        return true;
    };

    std::wstring standardInOutRedirectionFullFilename
        = NelsonConfiguration::getInstance()->getNelsonModulesDirectory()
        + L"/python_engine/resources/python/standardInOutRedirection.py";

    std::string content;
    if (!readFileIntoContent(standardInOutRedirectionFullFilename, content)) {
        return false;
    }

    NLSPy_Initialize();
    mainPythonInterpreter = NLSPyImport_AddModule("__main__");
    NLSPyRun_SimpleStringFlags(content.c_str(), NULL);
    return NLSPy_IsInitialized();
}
//=============================================================================
bool
PyFinalizeInterpreter(void)
{
    if (NLSPy_IsInitialized()) {
        NLSPy_Finalize();
    }
    return !NLSPy_IsInitialized();
}
//=============================================================================
std::wstring
getPythonStandardOutput()
{
    PyObject* StdOut = NLSPyObject_GetAttrString(mainPythonInterpreter, "stdout_catcher");
    PyObject* data = NLSPyObject_GetAttrString(StdOut, "data");
    std::string str(NLSPyUnicode_AsUTF8(data));
    std::wstring wstr = utf8_to_wstring(str);
    NLSPyRun_SimpleStringFlags("stdout_catcher.data = ''", NULL);
    return wstr;
}
//=============================================================================
std::wstring
getPythonStandardError()
{
    PyObject* StdErr = NLSPyObject_GetAttrString(mainPythonInterpreter, "stderr_catcher");
    PyObject* data = NLSPyObject_GetAttrString(StdErr, "data");
    std::string str(NLSPyUnicode_AsUTF8(data));
    std::wstring wstr = utf8_to_wstring(str);
    NLSPyRun_SimpleStringFlags("stderr_catcher.data = ''", NULL);
    return wstr;
}
//=============================================================================
void*
getPythonInterpreter(void)
{
    return (void*)mainPythonInterpreter;
}
//=============================================================================
void
initializePythonEngine()
{
    if (!isPythonLibraryLoaded()) {
        PythonEnvironment* pythonEnvironment = PythonEnvironment::getInstance();
        loadPythonLibrary(pythonEnvironment->getLibrary());
        if (!isPythonLibraryLoaded()) {
            pythonEnvironment->setStatus(0);
            Error(_W("Impossible to load Python library."));
        }
        pythonEnvironment->setStatus(1);
    }
    if (!PyIsInitializedInterpreter()) {
        if (!PyInitializeInterpreter()) {
            Error(_W("Cannot initialize python environment."));
        }
    }
}
//=============================================================================

}
//=============================================================================
