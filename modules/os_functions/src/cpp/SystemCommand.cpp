//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#include <io.h>
#include <windows.h>
#else
#include <sys/wait.h>
#include <unistd.h>
#endif
#include "SystemCommand.hpp"
#include "characters_encoding.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
deleteFile(boost::filesystem::path p)
{
    if (boost::filesystem::exists(p)) {
        try {
            boost::filesystem::remove(p);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                // ONLY FOR DEBUG
            }
        }
    }
}
//=============================================================================
ArrayOf
SystemCommand(const std::wstring& command, int& ierr)
{
    return ArrayOf::characterArrayConstructor(SystemCommandW(command, ierr));
}
//=============================================================================
static bool
DetectDetachProcess(std::wstring command)
{
    bool bRes = false;
    for (std::wstring::reverse_iterator rit = command.rbegin(); rit != command.rend(); ++rit) {
        if (*rit == L' ') {
            command.pop_back();
        } else {
            break;
        }
    }
    if (*command.rbegin() == L'&') {
        bRes = true;
    }
    return bRes;
}
//=============================================================================
static int
systemCall(const std::wstring& command)
{
    fflush(NULL);
#ifdef _MSC_VER
    int ierr = _wsystem(command.c_str());
#else
    std::string commandLineU = wstring_to_utf8(command);
    int ierr = system(commandLineU.c_str());
    // This macro queries the child termination status provided by the wait and waitpid functions.
    ierr = WEXITSTATUS(ierr);
#endif
    return ierr;
}
//=============================================================================
static std::wstring
SystemCommandDetachedW(const std::wstring& command, int& ierr)
{
    std::wstring commandLine;
#ifdef _MSC_VER
    commandLine = L"start cmd  /K " + command;
    commandLine.pop_back();
#else
    commandLine = command;
#endif
    ierr = systemCall(commandLine);
    ierr = 0;
    return std::wstring();
}
//=============================================================================
#ifdef _MSC_VER
static std::wstring
SystemCommandAttachedW_windows(const std::wstring& command, int& ierr)
{
#define BUFFER_POPEN 4096
    std::wstring result = L"";
    std::wstring commandwithredirection = L"\"" + command + L"\" 2>&1";
    FILE* pPipe = _wpopen(commandwithredirection.c_str(), L"rt");
    if (pPipe == nullptr) {
        Error(_W("Cannot call unix command."));
    } else {
        char psBuffer[BUFFER_POPEN];
        if (feof(pPipe)) {
            ierr = _pclose(pPipe);
            pPipe = nullptr;
        } else {
            if (command == L"start") {
                ierr = _pclose(pPipe);
                pPipe = nullptr;
            } else {
                while (fgets(psBuffer, BUFFER_POPEN, pPipe)) {
                    OemToAnsi(psBuffer, psBuffer);
                    result.append(utf8_to_wstring(psBuffer));
                }
                if (feof(pPipe)) {
                    ierr = _pclose(pPipe);
                    pPipe = nullptr;
                } else {
                    _pclose(pPipe);
                    pPipe = nullptr;
                    ierr = -1;
                }
            }
        }
    }
    return result;
}
//=============================================================================
#else
static std::wstring
SystemCommandAttachedW_others(const std::wstring& command, int& ierr)
{
    std::wstring result = L"";
    boost::filesystem::path pwd = boost::filesystem::temp_directory_path();
    boost::filesystem::path tempOutputFile = pwd;
    boost::filesystem::path tempErrorFile = pwd;
    boost::filesystem::path tempInputFile = pwd;
    tempOutputFile /= boost::filesystem::unique_path();
    tempErrorFile /= boost::filesystem::unique_path();
    tempInputFile /= boost::filesystem::unique_path();
    int stdoutBackup = dup(STDOUT_FILENO);
    int stderrBackup = dup(STDERR_FILENO);
    int stdinBackup = dup(STDIN_FILENO);
    if (stdoutBackup == -1) {
        Error(_W("Cannot duplicate stdout (1)."));
    }
    if (stdinBackup == -1) {
        Error(_W("Cannot duplicate stdin (1)."));
    }
    if (stderrBackup == -1) {
        Error(_W("Cannot duplicate stderr (1)."));
    }
    FILE* fpStdOutRedirected = freopen(tempOutputFile.generic_string().c_str(), "w", stdout);
    FILE* fpStdErrRedirected = freopen(tempErrorFile.generic_string().c_str(), "w", stderr);
    if (fpStdOutRedirected == nullptr) {
        Error(_W("Cannot redirect stdout."));
    }
    if (fpStdErrRedirected == nullptr) {
        Error(_W("Cannot redirect stderr."));
    }
    close(STDIN_FILENO);
    ierr = systemCall(command);
    close(STDOUT_FILENO);
    close(STDERR_FILENO);
    if (dup2(stdoutBackup, STDOUT_FILENO) == -1) {
        Error(_W("Cannot restore stdout."));
    }
    if (dup2(stderrBackup, STDERR_FILENO) == -1) {
        Error(_W("Cannot restore stdout."));
    }
    if (dup2(stdinBackup, STDIN_FILENO) == -1) {
        Error(_W("Cannot restore stdin."));
    }
    clearerr(stdout);
    clearerr(stdin);
    clearerr(stderr);
    if (close(stdoutBackup) == -1) {
        Error(_W("Cannot close redirected stdout."));
    }
    if (close(stderrBackup) == -1) {
        Error(_W("Cannot close redirected stderr."));
    }
    if (close(stdinBackup) == -1) {
        Error(_W("Cannot close redirected stdin."));
    }
    fflush(NULL);
    FILE* pFile = nullptr;
    if (ierr) {
        int fsize = 0;
        try {
            fsize = (int)boost::filesystem::file_size(tempErrorFile);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                // ONLY FOR DEBUG
            }
            fsize = 0;
        }
        if (fsize == 0) {
            pFile = fopen(tempOutputFile.string().c_str(), "r");
        } else {
            pFile = fopen(tempErrorFile.string().c_str(), "r");
        }
    } else {
        pFile = fopen(tempOutputFile.string().c_str(), "r");
    }
    if (pFile == nullptr) {
        deleteFile(tempOutputFile);
        deleteFile(tempErrorFile);
        deleteFile(tempInputFile);
        Error(_W("Cannot read results."));
    } else {
        std::string resultUTF = "";
        char buffer[4096];
        resultUTF.reserve(4096 * 2);
        while (fgets(buffer, sizeof(buffer), pFile)) {
            resultUTF.append(buffer);
        }
        result = utf8_to_wstring(resultUTF);
        if (result.size() > 0) {
            if (*result.rbegin() != L'\n') {
                result.append(L"\n");
            }
        }
        fclose(pFile);
    }
    deleteFile(tempErrorFile);
    deleteFile(tempOutputFile);
    deleteFile(tempInputFile);
    return result;
}
#endif
//=============================================================================
static std::wstring
SystemCommandAttachedW(const std::wstring& command, int& ierr)
{
#ifdef _MSC_VER
    return SystemCommandAttachedW_windows(command, ierr);
#else
    return SystemCommandAttachedW_others(command, ierr);
#endif
}
//=============================================================================
std::wstring
SystemCommandW(const std::wstring& command, int& ierr)
{
    std::wstring result = L"";
    bool bDetach = DetectDetachProcess(command);
    if (bDetach) {
        result = SystemCommandDetachedW(command, ierr);
    } else {
        result = SystemCommandAttachedW(command, ierr);
    }
    return result;
}
//=============================================================================
}
//=============================================================================
