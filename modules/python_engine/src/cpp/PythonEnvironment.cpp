//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
PythonEnvironment* PythonEnvironment::instance = nullptr;
//=============================================================================
PythonEnvironment::PythonEnvironment()
    : HandleGenericObject(NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR, this, false)
{
    propertiesNames
        = { L"Version", L"Executable", L"Library", L"Home", L"Status", L"ExecutionMode" };

    _status = 0;
    _executionMode = 0;
    loadPreviousState();
}
//=============================================================================
bool
PythonEnvironment::loadPreviousState()
{
    bool bOK = false;
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring pythonFile = prefDir + L"/python.conf";
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(pythonFile);
    if (bIsFile) {
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(pythonFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(pythonFile));
#endif
        if (jsonFile.is_open()) {
            nlohmann::json data;
            try {
                data = nlohmann::json::parse(jsonFile);
                std::string version = data["Version"];
                std::string executable = data["Executable"];
                std::string library = data["Library"];
                std::string home = data["Home"];

                std::wstring versionW = utf8_to_wstring(version);
                std::wstring executableW = utf8_to_wstring(executable);
                std::wstring libraryW = utf8_to_wstring(library);
                std::wstring homeW = utf8_to_wstring(home);
                if (FileSystemWrapper::Path::is_regular_file(executableW)
                    && FileSystemWrapper::Path::is_regular_file(libraryW)
                    && FileSystemWrapper::Path::is_directory(homeW)) {
                    setVersion(versionW);
                    setExecutable(executableW);
                    setLibrary(libraryW);
                    setHome(homeW);
                    setExecutionMode(data["ExecutionMode"]);
                    bOK = true;
                }
            } catch (const nlohmann::json::exception&) {
            }
            jsonFile.close();
        }
    }
    return bOK;
}
//=============================================================================
bool
PythonEnvironment::saveCurrentState()
{
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring pythonFile = prefDir + L"/python.conf";
    nlohmann::json data;

    data["Version"] = wstring_to_utf8(getVersion());
    data["Executable"] = wstring_to_utf8(getExecutable());
    data["Library"] = wstring_to_utf8(getLibrary());
    data["Home"] = wstring_to_utf8(getHome());
    data["ExecutionMode"] = getExecutionMode();
#ifdef _MSC_VER
    std::ofstream out(pythonFile);
#else
    std::ofstream out(wstring_to_utf8(pythonFile));
#endif
    if (out.is_open()) {
        out << data;
        out.close();
        return true;
    }
    return false;
}
//=============================================================================
std::wstring
PythonEnvironment::getVersion()
{
    return _version;
}
//=============================================================================
std::wstring
PythonEnvironment::getExecutable()
{
    return _executable;
}
//=============================================================================
std::wstring
PythonEnvironment::getLibrary()
{
    return _library;
}
//=============================================================================
std::wstring
PythonEnvironment::getHome()
{
    return _home;
}
//=============================================================================
int
PythonEnvironment::getStatus()
{
    return _status;
}
//=============================================================================
int
PythonEnvironment::getExecutionMode()
{
    return _executionMode;
}
//=============================================================================
void
PythonEnvironment::setVersion(const std::wstring& version)
{
    _version = version;
}
//=============================================================================
void
PythonEnvironment::setExecutable(const std::wstring& executable)
{
    _executable = executable;
}
//=============================================================================
void
PythonEnvironment::setLibrary(const std::wstring& library)
{
    _library = library;
}
//=============================================================================
void
PythonEnvironment::setHome(const std::wstring& home)
{
    _home = home;
}
//=============================================================================
void
PythonEnvironment::setStatus(int status)
{
    _status = status;
}
//=============================================================================
void
PythonEnvironment::setExecutionMode(int mode)
{
    _executionMode = mode;
}
//=============================================================================
wstringVector
PythonEnvironment::getProperties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
PythonEnvironment::getMethods()
{
    return {};
}
//=============================================================================
bool
PythonEnvironment::get(const std::wstring& propertyName, ArrayOf& result)
{
    if (propertyName == L"Version") {
        result = ArrayOf::stringArrayConstructor(getVersion());
        return true;
    }
    if (propertyName == L"Executable") {
        result = ArrayOf::stringArrayConstructor(getExecutable());
        return true;
    }
    if (propertyName == L"Library") {
        result = ArrayOf::stringArrayConstructor(getLibrary());
        return true;
    }
    if (propertyName == L"Home") {
        result = ArrayOf::stringArrayConstructor(getHome());
        return true;
    }
    if (propertyName == L"Status") {
        result = ArrayOf::doubleConstructor(getStatus());
        return true;
    }
    if (propertyName == L"ExecutionMode") {
        result = ArrayOf::doubleConstructor(getExecutionMode());
        return true;
    }
    return false;
}
//=============================================================================
void
PythonEnvironment::display(Interface* io)
{

    LineSpacingDisplay lineSpacingDisplay
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

#define BLANKS_AT_BOL std::wstring(L"   ")

    if (lineSpacingDisplay == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }

    io->outputMessage(BLANKS_AT_BOL + L"Version: \t\"" + getVersion() + L"\"\n");
    io->outputMessage(BLANKS_AT_BOL + L"Executable: \t\"" + getExecutable() + L"\"\n");
    io->outputMessage(BLANKS_AT_BOL + L"Library: \t\"" + getLibrary() + L"\"\n");
    io->outputMessage(BLANKS_AT_BOL + L"Home: \t\"" + getHome() + L"\"\n");

    std::wstring status;
    switch (getStatus()) {
    case 0: {
        status = L"NotLoaded";
    } break;
    case 1: {
        status = L"Loaded";
    } break;
    default: {
    } break;
    }
    io->outputMessage(BLANKS_AT_BOL + L"Status: \t" + status + L"\n");

    std::wstring executionMode;
    switch (getExecutionMode()) {
    case 0: {
        executionMode = L"InProcess";
    } break;
    case 1: {
        executionMode = L"OutOfProcess";
    } break;
    default: {
    } break;
    }
    io->outputMessage(BLANKS_AT_BOL + L"ExecutionMode: \t" + executionMode + L"\n");
}
//=============================================================================
void
configurePythonEnvironment(const std::wstring& executablePath)
{
}
//=============================================================================
}
//=============================================================================
