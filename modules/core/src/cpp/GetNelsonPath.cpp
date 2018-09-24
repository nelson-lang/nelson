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
#include <Windows.h>
#endif
#include <stdio.h>
#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#include <MacTypes.h>
#endif
#include "GetNelsonPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
using namespace boost::filesystem;
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring NelSonPath = L"";
//=============================================================================
#ifdef _MSC_VER
std::wstring
get_basepathW()
{
#define NELSON_ENGINE_DLL L"libnlsEngine.dll"
    std::wstring path;
    wchar_t buffer[MAX_PATH];
    GetModuleFileNameW(GetModuleHandleW(NELSON_ENGINE_DLL), buffer, MAX_PATH);
    path = buffer;
    return path;
}
//=============================================================================
#else
#ifdef __APPLE__
std::string
get_basepathU()
{
    char path[1024];
    CFBundleRef mainBundle = CFBundleGetMainBundle();
    if (!mainBundle) {
        return "";
    }
    CFURLRef mainBundleURL = CFBundleCopyBundleURL(mainBundle);
    if (!mainBundleURL) {
        return "";
    }
    CFStringRef cfStringRef = CFURLCopyFileSystemPath(mainBundleURL, kCFURLPOSIXPathStyle);
    if (!cfStringRef) {
        return "";
    }
    CFStringGetCString(cfStringRef, path, 1024, kCFStringEncodingASCII);
    CFRelease(mainBundleURL);
    CFRelease(cfStringRef);
    return std::string(path);
}
#else // LINUX
std::string
get_basepathU()
{
    std::string path = "";
    pid_t pid = getpid();
    char buf[10];
    sprintf(buf, "%d", pid);
    std::string _link = "/proc/";
    _link.append(buf);
    _link.append("/exe");
    char proc[512];
    int ch = readlink(_link.c_str(), proc, 512);
    if (ch != -1) {
        proc[ch] = 0;
        path = proc;
        std::string::size_type t = path.find_last_of("/");
        path = path.substr(0, t);
    }
    return (path);
}
#endif
#endif
//=============================================================================
ArrayOf
GetRootFolder()
{
    return ArrayOf::characterArrayConstructor(GetNelsonPath());
}
//=============================================================================
std::wstring
GetRootPath()
{
    if (NelSonPath == L"") {
        std::wstring p = L"";
#define NELSON_ROOT_PATH_ENV L"NELSON_ROOT_PATH"
        std::wstring penv = GetVariableEnvironment(NELSON_ROOT_PATH_ENV, L"");
        if (penv.compare(L"") != 0) {
            boost::filesystem::path path(penv);
            if (boost::filesystem::is_directory(path)) {
                NelSonPath = path.generic_wstring();
                return NelSonPath;
            }
        }
#ifdef _MSC_VER
        p = get_basepathW();
#else
        p = utf8_to_wstring(get_basepathU());
#endif
        boost::filesystem::path path(p);
        boost::filesystem::path nelsonpath;
#ifdef _MSC_VER
        nelsonpath = path.parent_path().parent_path().parent_path();
#else
        nelsonpath = path.parent_path().parent_path();
#endif
        if (boost::filesystem::is_directory(nelsonpath)) {
            NelSonPath = nelsonpath.generic_wstring();
            return NelSonPath;
        }
        fprintf(stderr, "%s\n", _("Error: we cannot find Nelson root path.").c_str());
        NelSonPath = std::wstring(L"");
    }
    return NelSonPath;
}
//=============================================================================
}
//=============================================================================
std::wstring
GetNelsonPath()
{
    return Nelson::GetRootPath();
}
//=============================================================================
