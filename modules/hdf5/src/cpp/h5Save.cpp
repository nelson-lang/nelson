//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <ctime>
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
#include "h5Save.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveVariable.hpp"
//=============================================================================
#if _MSC_VER
#if defined(_WIN64)
#define NLSFSEEK _fseeki64
#else
#define NLSFSEEK fseek
#endif
#else
#if defined(__APPLE__) || defined(__MACH__)
#define NLSFSEEK fseek
#else
#if defined(_LP64)
#if defined(HAVE_FSEEK64)
#define NLSFSEEK fseek64
#else
#define NLSFSEEK fseek
#endif
#else
#define NLSFSEEK fseek
#endif
#endif
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
createHeader()
{
    std::wstring header = std::wstring(NELSON_HEADER);
#ifdef _MSC_VER
    time_t _tm = time(nullptr);
    struct tm* curtime = localtime(&_tm);
    std::wstring timestr = utf8_to_wstring(asctime(curtime));
#else
    struct tm newtime;
    time_t ltime;
    char buf[128];
    ltime = time(&ltime);
    localtime_r(&ltime, &newtime);
    std::wstring timestr = utf8_to_wstring(asctime_r(&newtime, buf));
#endif
    StringHelpers::replace_last(timestr, L"\n", L"");
    return header + std::wstring(L" on ") + timestr;
}
//=============================================================================
static hid_t
createNh5FileWithHeader(const std::wstring& filename, const std::wstring& header)
{
    char* header_offset = nullptr;
    try {
        header_offset = new char[8];
        memset(header_offset, ' ', (size_t)8 * sizeof(char));
    } catch (std::bad_alloc&) {
        return H5I_INVALID_HID;
    }
    char* header_saturated = nullptr;
    try {
        header_saturated = new char[128];
        memset(header_saturated, ' ', (size_t)128 * sizeof(char));
    } catch (std::bad_alloc&) {
        delete[] header_offset;
        return H5I_INVALID_HID;
    }

    std::string utf_filename = wstring_to_utf8(filename);
    hid_t plist_id = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_userblock(plist_id, 512);
    hid_t plist_ap = H5Pcreate(H5P_FILE_ACCESS);
#if H5_VERSION_GE(1, 10, 2)
    H5Pset_libver_bounds(plist_ap, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18);
#endif
    hid_t fid = H5Fcreate(utf_filename.c_str(), H5F_ACC_TRUNC, plist_id, plist_ap);
    H5Fclose(fid);
    H5Pclose(plist_id);
    if (fid < 0) {
        delete[] header_offset;
        delete[] header_saturated;
        return fid;
    }
#ifdef _MSC_VER
    FILE* fp = _wfopen(filename.c_str(), L"r+b");
#else
    FILE* fp = fopen(utf_filename.c_str(), "r+b");
#endif
    if (fp == nullptr) {
        delete[] header_offset;
        delete[] header_saturated;
        return H5I_INVALID_HID;
    }
    (void)NLSFSEEK(fp, 0, SEEK_SET);
    size_t len = snprintf(header_saturated, 116, "%s", wstring_to_utf8(header).c_str());
    if (len >= 116) {
        header_saturated[115] = '\0';
    }

    int16 endian = NELSON_HEADER_ENDIAN;
    int16 version = NELSON_HEADER_VERSION;

    fwrite(header_saturated, 1, 116, fp);
    fwrite(header_offset, 1, 8, fp);
    fwrite(&version, 2, 1, fp);
    fwrite(&endian, 2, 1, fp);
    fclose(fp);

    fid = H5Fopen(utf_filename.c_str(), H5F_ACC_RDWR, plist_ap);
    H5Pclose(plist_ap);
    delete[] header_offset;
    delete[] header_saturated;
    return fid;
}
//=============================================================================
void
h5Save(Evaluator* eval, const std::wstring& filename, const wstringVector& names, bool append,
    bool nocompression)
{
    wstringVector variablesName;
    for (const auto& name : names) {

        if (!IsValidVariableName(name)) {
            Error(_W("Invalid variable name:") + name);
        }
        if (!eval->getContext()->isVariable(name)) {
            Error(_W("Variable does not exist:") + name);
        }
    }

    variablesName = names;
    if (variablesName.empty()) {
        eval->getContext()->getCurrentScope()->getVariablesList(false, variablesName);
    }

    hid_t fid = H5I_INVALID_HID;
    FileSystemWrapper::Path hdf5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
    }
    if (!fileExistPreviously) {
        fid = createNh5FileWithHeader(hdf5_filename.wstring(), createHeader());
    } else {
        if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str())) {
            Error(_W("HDF5 format file expected."));
        } else {
            if (append) {
                fid = H5Fopen(
                    wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
            } else {
                FileSystemWrapper::Path p(hdf5_filename);
                if (!FileSystemWrapper::Path::remove(p)) {
                    Error(_W("Cannot replace file"));
                }
                fid = createNh5FileWithHeader(hdf5_filename.wstring(), createHeader());
                updateNelsonH5Header(fid);
            }
        }
    }

    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (fileExistPreviously) {
        if (!isNelsonH5File(fid)) {
            H5Fclose(fid);
            Error(_W("Invalid file format."));
        }
        if (append) {
            int32 schema = getNelsonH5Schema(fid);
            bool isUnmanaged = (schema != NELSON_SCHEMA);
            if (isUnmanaged) {
                Error(_W("Invalid file version."));
                H5Fclose(fid);
            }
        }
    }
    if (!fileExistPreviously) {
        updateNelsonH5Header(fid);
    }

    std::string location = "/";
    for (auto& k : variablesName) {
        ArrayOf variableValue;
        std::string variableName = wstring_to_utf8(k);
        eval->getContext()->getCurrentScope()->lookupVariable(variableName, variableValue);
        bool bSuccess = h5SaveVariable(fid, location, variableName, variableValue, !nocompression);
        if (!bSuccess) {
            H5Fclose(fid);
            Error(_("Cannot save variable:") + variableName);
        }
    }
    H5Fclose(fid);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
