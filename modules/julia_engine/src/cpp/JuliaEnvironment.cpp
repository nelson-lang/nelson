//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
JuliaEnvironment* JuliaEnvironment::instance = nullptr;
//=============================================================================
JuliaEnvironment::JuliaEnvironment()
    : HandleGenericObject(NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR, this, false)
{
    propertiesNames
        = { L"Version", L"Executable", L"Library", L"Home", L"Status", L"ExecutionMode" };

    _status = 0;
    _executionMode = 0;
    if (!loadPreviousStateFromEnvironmentVariables()) {
        loadPreviousState();
    }
}
//=============================================================================
bool
JuliaEnvironment::loadPreviousStateFromEnvironmentVariables()
{
    std::wstring juliaVersion = GetVariableEnvironment(L"__NELSON_JULIA_VERSION__", L"");
    if (juliaVersion.empty()) {
        return false;
    }
    std::wstring juliaExecutable = GetVariableEnvironment(L"__NELSON_JULIA_EXECUTABLE__", L"");
    if (!FileSystemWrapper::Path::is_regular_file(juliaExecutable)) {
        return false;
    }
    std::wstring juliaLibrary = GetVariableEnvironment(L"__NELSON_JULIA_LIBRARY__", L"");
    if (juliaLibrary.empty()) {
        return false;
    }
    std::wstring juliaHome = GetVariableEnvironment(L"__NELSON_JULIA_HOME__", L"");
    if (!FileSystemWrapper::Path::is_directory(juliaHome)) {
        return false;
    }
    setVersion(juliaVersion);
    setExecutable(juliaExecutable);
    setLibrary(juliaLibrary);
    setHome(juliaHome);
    return true;
}
//=============================================================================
bool
JuliaEnvironment::loadPreviousState()
{
    bool bOK = false;
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring juliaFile = prefDir + L"/julia.conf";
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(juliaFile);
    if (bIsFile) {
#ifdef _MSC_VER
        std::ifstream jsonFile(juliaFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(juliaFile));
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
                if (FileSystemWrapper::Path::is_regular_file(executableW) && !libraryW.empty()
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
JuliaEnvironment::saveCurrentState()
{
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring juliaFile = prefDir + L"/julia.conf";
    nlohmann::json data;

    data["Version"] = wstring_to_utf8(getVersion());
    data["Executable"] = wstring_to_utf8(getExecutable());
    data["Library"] = wstring_to_utf8(getLibrary());
    data["Home"] = wstring_to_utf8(getHome());
    data["ExecutionMode"] = getExecutionMode();
#ifdef _MSC_VER
    std::ofstream out(juliaFile);
#else
    std::ofstream out(wstring_to_utf8(juliaFile));
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
JuliaEnvironment::getVersion() const
{
    return _version;
}
//=============================================================================
std::wstring
JuliaEnvironment::getExecutable() const
{
    return _executable;
}
//=============================================================================
std::wstring
JuliaEnvironment::getLibrary() const
{
    return _library;
}
//=============================================================================
std::wstring
JuliaEnvironment::getHome() const
{
    return _home;
}
//=============================================================================
int
JuliaEnvironment::getStatus() const
{
    return _status;
}
//=============================================================================
int
JuliaEnvironment::getExecutionMode() const
{
    return _executionMode;
}
//=============================================================================
void
JuliaEnvironment::setVersion(const std::wstring& version)
{
    _version = version;
}
//=============================================================================
void
JuliaEnvironment::setExecutable(const std::wstring& executable)
{
    _executable = executable;
}
//=============================================================================
void
JuliaEnvironment::setLibrary(const std::wstring& library)
{
    _library = library;
}
//=============================================================================
void
JuliaEnvironment::setHome(const std::wstring& home)
{
    _home = home;
}
//=============================================================================
void
JuliaEnvironment::setStatus(int status)
{
    _status = status;
}
//=============================================================================
void
JuliaEnvironment::setExecutionMode(int mode)
{
    _executionMode = mode;
}
//=============================================================================
wstringVector
JuliaEnvironment::getProperties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
JuliaEnvironment::getMethods()
{
    return {};
}
//=============================================================================
std::wstring
JuliaEnvironment::getStatusString()
{
    switch (getStatus()) {
    case 0: {
        return L"NotLoaded";
    } break;
    case 1: {
        return L"Loaded";
    } break;
    case 2: {
        return L"Terminated";
    } break;
    default: {
    } break;
    }
    return L"ERROR";
}
//=============================================================================
std::wstring
JuliaEnvironment::getExecutionModeString()
{
    switch (getExecutionMode()) {
    case 0: {
        return L"InProcess";
    } break;
    case 1: {
        return L"OutOfProcess";
    } break;
    default: {
    } break;
    }
    return L"ERROR";
}
//=============================================================================
bool
JuliaEnvironment::get(const std::wstring& propertyName, ArrayOf& result)
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
        result = ArrayOf::stringArrayConstructor(getStatusString());
        return true;
    }
    if (propertyName == L"ExecutionMode") {
        result = ArrayOf::stringArrayConstructor(getExecutionModeString());
        return true;
    }
    return false;
}
//=============================================================================
void
JuliaEnvironment::display(Interface* io)
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

    std::wstring status = getStatusString();
    io->outputMessage(BLANKS_AT_BOL + L"Status: \t" + status + L"\n");

    std::wstring executionMode = getExecutionModeString();
    io->outputMessage(BLANKS_AT_BOL + L"ExecutionMode: \t" + executionMode + L"\n");
}
//=============================================================================
bool
configureJuliaEnvironment(const JuliaEnvironment* juliaEnvironment)
{
#ifdef _MSC_VER
    std::wstring libraryName = juliaEnvironment->getLibrary();
    if (libraryName.empty()) {
        return false;
    }
    size_t lastSlash = libraryName.find_last_of(L"/\\");
    std::wstring directoryLibraryPath = libraryName.substr(0, lastSlash);

    std::wstring pathEnv = GetVariableEnvironment(L"PATH", L"");
    std::wstring delimiter = L";";

    size_t start = 0, end = 0;
    bool alreadyInPathEnv = false;
    while ((end = pathEnv.find(delimiter, start)) != std::wstring::npos) {
        if (pathEnv.substr(start, end - start) == directoryLibraryPath) {
            alreadyInPathEnv = true;
            break;
        }
        start = end + delimiter.length();
    }
    alreadyInPathEnv = !alreadyInPathEnv && (pathEnv.substr(start) == directoryLibraryPath);
    if (!alreadyInPathEnv) {
        std::wstring newPathEnv;
        if (!pathEnv.empty() && pathEnv.back() == L';') {
            newPathEnv = pathEnv + directoryLibraryPath + L";";
        } else {
            newPathEnv = pathEnv + L";" + directoryLibraryPath + L";";
        }
        SetVariableEnvironmentW(L"PATH", newPathEnv);
    }
#endif
    return false;
}
//=============================================================================
}
//=============================================================================
