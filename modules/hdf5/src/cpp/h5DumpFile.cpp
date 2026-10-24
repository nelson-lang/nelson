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
#include <iomanip>
#include <sstream>
#include <vector>
#include "h5DumpFile.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static hid_t
openValidHdf5File(const std::wstring& filename, std::string& utf8Filename)
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
    utf8Filename = wstring_to_utf8(hdf5Filename.wstring());
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
static std::string
indent(size_t level)
{
    return std::string(level * 3, ' ');
}
//=============================================================================
static std::string
basename(const std::string& path)
{
    if (path == "/") {
        return "/";
    }
    size_t pos = path.find_last_of('/');
    return pos == std::string::npos ? path : path.substr(pos + 1);
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
static std::vector<hsize_t>
getSpaceDimensions(hid_t space, int& rank)
{
    rank = H5Sget_simple_extent_ndims(space);
    if (rank <= 0) {
        return {};
    }
    std::vector<hsize_t> dims(static_cast<size_t>(rank));
    H5Sget_simple_extent_dims(space, dims.data(), nullptr);
    return dims;
}
//=============================================================================
static size_t
elementCount(const std::vector<hsize_t>& dims, int rank)
{
    if (rank == 0) {
        return 1;
    }
    size_t count = 1;
    for (hsize_t dim : dims) {
        count *= static_cast<size_t>(dim);
    }
    return count;
}
//=============================================================================
static std::string
formatDimensions(const std::vector<hsize_t>& dims, int rank)
{
    if (rank == 0) {
        return "SCALAR";
    }
    std::ostringstream stream;
    stream << "SIMPLE { ( ";
    for (int i = 0; i < rank; ++i) {
        if (i > 0) {
            stream << ", ";
        }
        stream << dims[static_cast<size_t>(i)];
    }
    stream << " ) / ( ";
    for (int i = 0; i < rank; ++i) {
        if (i > 0) {
            stream << ", ";
        }
        stream << dims[static_cast<size_t>(i)];
    }
    stream << " ) }";
    return stream.str();
}
//=============================================================================
static std::string
formatIndex(size_t linearIndex, const std::vector<hsize_t>& dims, int rank)
{
    if (rank == 0) {
        return "(0)";
    }
    std::vector<hsize_t> indices(static_cast<size_t>(rank), 0);
    for (int i = rank - 1; i >= 0; --i) {
        hsize_t dim = dims[static_cast<size_t>(i)];
        indices[static_cast<size_t>(i)] = static_cast<hsize_t>(linearIndex % dim);
        linearIndex /= static_cast<size_t>(dim);
    }
    std::ostringstream stream;
    stream << "(";
    for (int i = 0; i < rank; ++i) {
        if (i > 0) {
            stream << ",";
        }
        stream << indices[static_cast<size_t>(i)];
    }
    stream << ")";
    return stream.str();
}
//=============================================================================
static std::string
numericTypeName(hid_t type)
{
    H5T_class_t typeClass = H5Tget_class(type);
    size_t typeSize = H5Tget_size(type);
    H5T_order_t order = H5Tget_order(type);
    std::string suffix = order == H5T_ORDER_BE ? "BE" : "LE";
    if (typeClass == H5T_FLOAT) {
        if (typeSize == 4) {
            return "H5T_IEEE_F32" + suffix;
        }
        if (typeSize == 8) {
            return "H5T_IEEE_F64" + suffix;
        }
    }
    if (typeClass == H5T_INTEGER) {
        H5T_sign_t sign = H5Tget_sign(type);
        std::string signName = sign == H5T_SGN_NONE ? "U" : "I";
        return "H5T_STD_" + signName + std::to_string(typeSize * 8) + suffix;
    }
    return "H5T_NATIVE_OPAQUE";
}
//=============================================================================
static std::string
escapeString(const std::string& value)
{
    std::string result;
    result.reserve(value.size());
    for (char c : value) {
        if (c == '\\' || c == '"') {
            result.push_back('\\');
        }
        result.push_back(c);
    }
    return result;
}
//=============================================================================
static std::string
readFixedString(const char* data, size_t offset, size_t size)
{
    std::string value(data + offset, data + offset + size);
    size_t nul = value.find('\0');
    if (nul != std::string::npos) {
        value.resize(nul);
    }
    return value;
}
//=============================================================================
static void
dumpStringData(std::ostringstream& stream, hid_t container, hid_t type, hid_t space, bool attribute,
    size_t level)
{
    int rank = 0;
    std::vector<hsize_t> dims = getSpaceDimensions(space, rank);
    size_t count = elementCount(dims, rank);
    stream << indent(level) << "DATA {\n";
    if (H5Tis_variable_str(type)) {
        std::vector<char*> values(count, nullptr);
        herr_t status = attribute
            ? H5Aread(container, type, values.data())
            : H5Dread(container, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, values.data());
        if (status >= 0) {
            for (size_t i = 0; i < count; ++i) {
                stream << indent(level) << formatIndex(i, dims, rank) << ": \""
                       << escapeString(values[i] == nullptr ? "" : values[i]) << "\"\n";
            }
            H5Dvlen_reclaim(type, space, H5P_DEFAULT, values.data());
        }
    } else {
        size_t typeSize = H5Tget_size(type);
        std::vector<char> values(count * typeSize, '\0');
        herr_t status = attribute
            ? H5Aread(container, type, values.data())
            : H5Dread(container, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, values.data());
        if (status >= 0) {
            for (size_t i = 0; i < count; ++i) {
                stream << indent(level) << formatIndex(i, dims, rank) << ": \""
                       << escapeString(readFixedString(values.data(), i * typeSize, typeSize))
                       << "\"\n";
            }
        }
    }
    stream << indent(level) << "}\n";
}
//=============================================================================
static void
dumpNumericData(std::ostringstream& stream, hid_t container, hid_t type, hid_t space,
    bool attribute, size_t level)
{
    int rank = 0;
    std::vector<hsize_t> dims = getSpaceDimensions(space, rank);
    size_t count = elementCount(dims, rank);
    std::vector<double> values(count, 0);
    herr_t status = attribute
        ? H5Aread(container, H5T_NATIVE_DOUBLE, values.data())
        : H5Dread(container, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, values.data());
    stream << indent(level) << "DATA {\n";
    if (status >= 0) {
        for (size_t i = 0; i < count; ++i) {
            stream << indent(level) << formatIndex(i, dims, rank) << ": " << std::setprecision(15)
                   << values[i] << "\n";
        }
    }
    stream << indent(level) << "}\n";
}
//=============================================================================
static void
dumpDatatype(std::ostringstream& stream, hid_t type, size_t level)
{
    if (H5Tget_class(type) == H5T_STRING) {
        stream << indent(level) << "DATATYPE  H5T_STRING {\n";
        if (H5Tis_variable_str(type)) {
            stream << indent(level + 1) << "STRSIZE H5T_VARIABLE;\n";
        } else {
            stream << indent(level + 1) << "STRSIZE " << H5Tget_size(type) << ";\n";
        }
        stream << indent(level) << "}\n";
    } else {
        stream << indent(level) << "DATATYPE  " << numericTypeName(type) << "\n";
    }
}
//=============================================================================
static void
dumpAttribute(std::ostringstream& stream, hid_t obj, const std::string& name, size_t level)
{
    hid_t attr = H5Aopen(obj, name.c_str(), H5P_DEFAULT);
    if (attr < 0) {
        return;
    }
    hid_t type = H5Aget_type(attr);
    hid_t space = H5Aget_space(attr);
    int rank = 0;
    std::vector<hsize_t> dims = getSpaceDimensions(space, rank);
    stream << indent(level) << "ATTRIBUTE \"" << name << "\" {\n";
    dumpDatatype(stream, type, level + 1);
    stream << indent(level + 1) << "DATASPACE  " << formatDimensions(dims, rank) << "\n";
    if (H5Tget_class(type) == H5T_STRING) {
        dumpStringData(stream, attr, type, space, true, level + 1);
    } else if (H5Tget_class(type) == H5T_FLOAT || H5Tget_class(type) == H5T_INTEGER) {
        dumpNumericData(stream, attr, type, space, true, level + 1);
    }
    stream << indent(level) << "}\n";
    H5Sclose(space);
    H5Tclose(type);
    H5Aclose(attr);
}
//=============================================================================
static void
dumpAttributes(std::ostringstream& stream, hid_t obj, size_t level)
{
    for (const std::string& attributeName : attributeNames(obj)) {
        dumpAttribute(stream, obj, attributeName, level);
    }
}
//=============================================================================
static void
dumpObject(std::ostringstream& stream, hid_t obj, const std::string& path, size_t level);
//=============================================================================
static void
dumpDataset(std::ostringstream& stream, hid_t dset, const std::string& path, size_t level)
{
    hid_t type = H5Dget_type(dset);
    hid_t space = H5Dget_space(dset);
    int rank = 0;
    std::vector<hsize_t> dims = getSpaceDimensions(space, rank);
    stream << indent(level) << "DATASET \"" << basename(path) << "\" {\n";
    dumpAttributes(stream, dset, level + 1);
    dumpDatatype(stream, type, level + 1);
    stream << indent(level + 1) << "DATASPACE  " << formatDimensions(dims, rank) << "\n";
    if (H5Tget_class(type) == H5T_STRING) {
        dumpStringData(stream, dset, type, space, false, level + 1);
    } else if (H5Tget_class(type) == H5T_FLOAT || H5Tget_class(type) == H5T_INTEGER) {
        dumpNumericData(stream, dset, type, space, false, level + 1);
    }
    stream << indent(level) << "}\n";
    H5Sclose(space);
    H5Tclose(type);
}
//=============================================================================
static void
dumpGroup(std::ostringstream& stream, hid_t group, const std::string& path, size_t level)
{
    stream << indent(level) << "GROUP \"" << basename(path) << "\" {\n";
    dumpAttributes(stream, group, level + 1);
    for (const std::string& childName : getLinkNames(group)) {
        H5O_type_t childType = getObjectType(group, childName);
        if (childType != H5O_TYPE_GROUP && childType != H5O_TYPE_DATASET) {
            continue;
        }
        hid_t child = H5Oopen(group, childName.c_str(), H5P_DEFAULT);
        if (child < 0) {
            continue;
        }
        dumpObject(stream, child, joinPath(path, childName), level + 1);
        H5Oclose(child);
    }
    stream << indent(level) << "}\n";
}
//=============================================================================
static void
dumpObject(std::ostringstream& stream, hid_t obj, const std::string& path, size_t level)
{
    H5O_info_t info;
#if H5_VERSION_GE(1, 12, 0)
    if (H5Oget_info3(obj, &info, H5O_INFO_BASIC) < 0) {
#else
    if (H5Oget_info(obj, &info) < 0) {
#endif
        return;
    }
    if (info.type == H5O_TYPE_GROUP) {
        dumpGroup(stream, obj, path, level);
    } else if (info.type == H5O_TYPE_DATASET) {
        dumpDataset(stream, obj, path, level);
    }
}
//=============================================================================
std::string
h5DumpFile(const std::wstring& filename, const std::wstring& location)
{
    std::string utf8Filename;
    hid_t fid = openValidHdf5File(filename, utf8Filename);
    std::string h5Location = normalizeLocation(location);
    hid_t obj = H5Oopen(fid, h5Location.c_str(), H5P_DEFAULT);
    if (obj < 0) {
        H5Fclose(fid);
        Error(L"Nelson:hdf5:locationNotOpened",
            L"Specified HDF5 object location could not be opened.");
    }
    std::ostringstream stream;
    stream << "HDF5 \"" << utf8Filename << "\" {\n";
    dumpObject(stream, obj, h5Location, 0);
    stream << "}\n";
    H5Oclose(obj);
    H5Fclose(fid);
    return stream.str();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
