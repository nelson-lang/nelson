//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include <algorithm>
#include "h5LsFile.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
normalizeLocation(const std::wstring& location)
{
    if (location.empty()) {
        return "/";
    }
    std::string utf8Location = wstring_to_utf8(location);
    if (utf8Location.empty() || utf8Location.front() != '/') {
        Error(L"Nelson:hdf5:invalidLocationPath", L"Second argument must start with '/'.");
    }
    if (utf8Location.size() > 1 && utf8Location.back() == '/') {
        utf8Location.pop_back();
    }
    return utf8Location;
}
//=============================================================================
static hid_t
openValidHdf5File(const std::wstring& filename)
{
    if (filename.empty()) {
        Error(L"Nelson:hdf5:invalidFilename", L"Valid %s expected.", L"filename");
    }
    FileSystemWrapper::Path hdf5Filename(filename);
    bool permissionDenied = false;
    bool fileExists = FileSystemWrapper::Path::is_regular_file(hdf5Filename, permissionDenied);
    if (!fileExists) {
        if (permissionDenied) {
            Error(L"Nelson:hdf5:permissionDenied", L"Permission denied.");
        }
        Error(L"Nelson:hdf5:fileDoesNotExist", L"File does not exist.");
    }
    std::string utf8Filename = wstring_to_utf8(hdf5Filename.wstring());
    if (!H5Fis_hdf5(utf8Filename.c_str())) {
        Error(L"Nelson:hdf5:notHdf5File", L"HDF5 format file expected.");
    }
    hid_t fid = H5Fopen(utf8Filename.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    if (fid == H5I_INVALID_HID) {
        Error(L"Nelson:hdf5:openFailed", L"Open file failed.");
    }
    return fid;
}
//=============================================================================
static std::string
joinPath(const std::string& parent, const std::string& name)
{
    if (parent == "/") {
        return "/" + name;
    }
    return parent + "/" + name;
}
//=============================================================================
static std::string
relativePath(const std::string& path, const std::string& base)
{
    if (base == "/") {
        return path;
    }
    if (path == base) {
        return "/";
    }
    if (path.rfind(base + "/", 0) == 0) {
        return path.substr(base.size());
    }
    return path;
}
//=============================================================================
static std::vector<std::string>
attributeNames(hid_t obj)
{
    std::vector<std::string> names;
    H5O_info_t info;
#if H5_VERSION_GE(1, 12, 0)
    if (H5Oget_info3(obj, &info, H5O_INFO_ALL) < 0) {
#else
    if (H5Oget_info(obj, &info) < 0) {
#endif
        return names;
    }
    names.reserve(static_cast<size_t>(info.num_attrs));
    for (hsize_t i = 0; i < info.num_attrs; ++i) {
        ssize_t len
            = H5Aget_name_by_idx(obj, ".", H5_INDEX_NAME, H5_ITER_INC, i, nullptr, 0, H5P_DEFAULT);
        if (len < 0) {
            continue;
        }
        std::string name(static_cast<size_t>(len) + 1, '\0');
        H5Aget_name_by_idx(
            obj, ".", H5_INDEX_NAME, H5_ITER_INC, i, name.data(), name.size(), H5P_DEFAULT);
        name.resize(static_cast<size_t>(len));
        names.push_back(name);
    }
    std::sort(names.begin(), names.end());
    return names;
}
//=============================================================================
static H5O_type_t
getObjectType(hid_t loc, const std::string& name)
{
    H5O_info_t objectInfo;
#if H5_VERSION_GE(1, 12, 0)
    if (H5Oget_info_by_name3(loc, name.c_str(), &objectInfo, H5O_INFO_BASIC, H5P_DEFAULT) < 0) {
#else
    if (H5Oget_info_by_name(loc, name.c_str(), &objectInfo, H5P_DEFAULT) < 0) {
#endif
        return H5O_TYPE_UNKNOWN;
    }
    return objectInfo.type;
}
//=============================================================================
static std::vector<std::string>
getLinkNames(hid_t group)
{
    std::vector<std::string> names;
    H5G_info_t info;
    if (H5Gget_info(group, &info) < 0) {
        return names;
    }
    names.reserve(static_cast<size_t>(info.nlinks));
    for (hsize_t i = 0; i < info.nlinks; ++i) {
        ssize_t len = H5Lget_name_by_idx(
            group, ".", H5_INDEX_NAME, H5_ITER_INC, i, nullptr, 0, H5P_DEFAULT);
        if (len < 0) {
            continue;
        }
        std::string name(static_cast<size_t>(len) + 1, '\0');
        H5Lget_name_by_idx(
            group, ".", H5_INDEX_NAME, H5_ITER_INC, i, name.data(), name.size(), H5P_DEFAULT);
        name.resize(static_cast<size_t>(len));
        names.push_back(name);
    }
    std::sort(names.begin(), names.end());
    return names;
}
//=============================================================================
static void
appendObjectEntries(hid_t obj, const std::string& path, const std::string& base,
    std::vector<H5LsEntry>& entries, bool includeSelf)
{
    H5O_info_t info;
#if H5_VERSION_GE(1, 12, 0)
    if (H5Oget_info3(obj, &info, H5O_INFO_BASIC) < 0) {
#else
    if (H5Oget_info(obj, &info) < 0) {
#endif
        return;
    }

    if (includeSelf) {
        if (info.type == H5O_TYPE_GROUP) {
            entries.push_back({ relativePath(path, base), "Group" });
        } else if (info.type == H5O_TYPE_DATASET) {
            entries.push_back({ relativePath(path, base), "Dataset" });
        }
    }

    for (const std::string& attributeName : attributeNames(obj)) {
        std::string attributePath
            = path == "/" ? attributeName : relativePath(path + "/" + attributeName, base);
        entries.push_back({ attributePath, "Attribute" });
    }

    if (info.type != H5O_TYPE_GROUP) {
        return;
    }

    for (const std::string& childName : getLinkNames(obj)) {
        H5O_type_t childType = getObjectType(obj, childName);
        if (childType != H5O_TYPE_GROUP && childType != H5O_TYPE_DATASET) {
            continue;
        }
        std::string childPath = joinPath(path, childName);
        hid_t child = H5Oopen(obj, childName.c_str(), H5P_DEFAULT);
        if (child < 0) {
            continue;
        }
        appendObjectEntries(child, childPath, base, entries, true);
        H5Oclose(child);
    }
}
//=============================================================================
std::vector<H5LsEntry>
h5LsFile(const std::wstring& filename, const std::wstring& location)
{
    std::string h5Location = normalizeLocation(location);
    hid_t fid = openValidHdf5File(filename);
    hid_t obj = H5Oopen(fid, h5Location.c_str(), H5P_DEFAULT);
    if (obj < 0) {
        H5Fclose(fid);
        Error(L"Nelson:hdf5:locationNotOpened",
            L"Specified HDF5 object location could not be opened.");
    }
    std::vector<H5LsEntry> entries;
    appendObjectEntries(obj, h5Location, h5Location, entries, h5Location == "/");
    H5Oclose(obj);
    H5Fclose(fid);
    return entries;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
