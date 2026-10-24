//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "h5SaveLoadHelpers.hpp"
#include <cstdio>
#include <cstring>
#include <ctime>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileSystemWrapper.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
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
hid_t
createNh5FileWithHeader(const std::wstring& filename)
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
        H5Pclose(plist_ap);
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
        H5Pclose(plist_ap);
        return H5I_INVALID_HID;
    }
    (void)NLSFSEEK(fp, 0, SEEK_SET);
    std::string header = wstring_to_utf8(createHeader());
    size_t len = snprintf(header_saturated, 116, "%s", header.c_str());
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
bool
isNelsonH5File(hid_t fid)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, "/", H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        Error(L"Nelson:hdf5:locationNotOpened",
            L"Specified HDF5 object location could not be opened.");
    }
    htri_t exists = H5Aexists(obj_id, NELSON_SCHEMA_STR);
    if (exists > 0) {
        bSuccess = true;
    }
    H5Oclose(obj_id);
    return bSuccess;
}
//=============================================================================
int32
getNelsonH5Schema(hid_t fid)
{
    int32 schema = -1;
    hid_t obj_id = H5Oopen(fid, "/", H5P_DEFAULT);
    if (obj_id < 0) {
        return schema;
    }
    hid_t attr_id = H5Aopen_by_name(obj_id, "/", NELSON_SCHEMA_STR, H5P_DEFAULT, H5P_DEFAULT);
    if (attr_id < 0) {
        H5Oclose(obj_id);
        return schema;
    }
    herr_t status = H5Aread(attr_id, H5T_NATIVE_INT32, &schema);
    if (status < 0) {
        schema = -1;
    }
    H5Aclose(attr_id);
    H5Oclose(obj_id);
    return schema;
}
//=============================================================================
bool
addSchemaFormat(hid_t obj_id)
{
    bool bSuccess = true;
    herr_t status;
    void* buffer = nullptr;
    hid_t att_id = H5I_INVALID_HID;
    hid_t dspace_id = H5Screate(H5S_SCALAR);
    hid_t type_id = H5Tcopy(H5T_NATIVE_INT32);
    att_id = H5Acreate(obj_id, NELSON_SCHEMA_STR, type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT);
    if (att_id < 0) {
        bSuccess = false;
    } else {
        int32 schema_version = NELSON_SCHEMA;
        buffer = &schema_version;
        status = H5Awrite(att_id, type_id, buffer);
        if (status < 0) {
            bSuccess = false;
        }
        H5Aclose(att_id);
    }
    H5Sclose(dspace_id);
    return bSuccess;
}
//=============================================================================
bool
updateNelsonH5Header(hid_t fid)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, "/", H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        Error(L"Nelson:hdf5:locationNotOpened",
            L"Specified HDF5 object location could not be opened.");
    }
    if (!addSchemaFormat(obj_id)) {
        H5Oclose(obj_id);
        return false;
    }
    H5Oclose(obj_id);
    return bSuccess;
}
//=============================================================================
stringVector
getVariableNames(hid_t fid)
{
    stringVector variableNames;
    H5G_info_t group_info;
    if (H5Gget_info(fid, &group_info) != 0) {
        return variableNames;
    }
    for (hsize_t i = 0; i < group_info.nlinks; i++) {
        H5O_info_t object_info;
#if H5_VERSION_GE(1, 12, 0)
        H5Oget_info_by_idx3(
            fid, ".", H5_INDEX_NAME, H5_ITER_INC, i, &object_info, H5O_INFO_BASIC, H5P_DEFAULT);
#else
        H5Oget_info_by_idx(fid, ".", H5_INDEX_NAME, H5_ITER_INC, i, &object_info, H5P_DEFAULT);
#endif
        if (object_info.type == H5O_TYPE_DATASET || object_info.type == H5O_TYPE_GROUP) {
            char* varName = nullptr;
            size_t sLen = 0;
            sLen = (size_t)H5Lget_name_by_idx(
                fid, ".", H5_INDEX_NAME, H5_ITER_INC, i, nullptr, sLen, H5P_DEFAULT);

            try {
                varName = new char[sLen + 1];
                H5Lget_name_by_idx(
                    fid, ".", H5_INDEX_NAME, H5_ITER_INC, i, varName, sLen + 1, H5P_DEFAULT);
                variableNames.push_back(varName);
                delete[] varName;
            } catch (const std::bad_alloc&) {
            }
        }
    }
    return variableNames;
}
//=============================================================================
static bool
attributeAsBool(hid_t fid, const std::string& location, const std::string& variableName,
    const std::string& attributeName)
{
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }

    hid_t obj_id = H5Oopen(fid, h5path.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return false;
    }
    htri_t exists = H5Aexists(obj_id, attributeName.c_str());
    if (exists <= 0) {
        H5Oclose(obj_id);
        return false;
    }
    hid_t attr_id
        = H5Aopen_by_name(obj_id, h5path.c_str(), attributeName.c_str(), H5P_DEFAULT, H5P_DEFAULT);
    if (attr_id < 0) {
        H5Oclose(obj_id);
        return false;
    }
    uint8 value = 0;
    herr_t status = H5Aread(attr_id, H5T_NATIVE_UINT8, &value);
    if (status < 0) {
        return false;
    }
    H5Aclose(attr_id);
    H5Oclose(obj_id);
    return value == 0 ? false : true != 0;
}
//=============================================================================
std::string
getNelsonClass(hid_t fid, const std::string& location, const std::string& variableName)
{
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    hid_t obj_id = H5Oopen(fid, h5path.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return "";
    }
    htri_t exists = H5Aexists(obj_id, NELSON_CLASS_STR);
    if (exists <= 0) {
        H5Oclose(obj_id);
        return "";
    }
    hsize_t dims[1];
    hid_t attr_id
        = H5Aopen_by_name(obj_id, h5path.c_str(), NELSON_CLASS_STR, H5P_DEFAULT, H5P_DEFAULT);
    hid_t aspace = H5Aget_space(attr_id);
    herr_t status = H5Sget_simple_extent_dims(aspace, dims, nullptr);
    if (status < 0) {
        H5Aclose(attr_id);
        H5Sclose(aspace);
        H5Oclose(obj_id);
        return "";
    }
    hid_t type = H5Aget_type(attr_id);
    hsize_t sDim = H5Tget_size(type);
    H5Tclose(type);
    char* pClassname = new char[(size_t(sDim + 1))];

    hid_t memtype = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(memtype, (size_t)sDim);
    if (status < 0) {
        if (pClassname) {
            delete[] pClassname;
            pClassname = nullptr;
        }
        H5Aclose(attr_id);
        H5Tclose(memtype);
        H5Sclose(aspace);
        H5Oclose(obj_id);
        return "";
    }

    status = H5Aread(attr_id, memtype, pClassname);
    if (status < 0) {
        if (pClassname) {
            delete[] pClassname;
            pClassname = nullptr;
        }
        H5Tclose(memtype);
        H5Sclose(aspace);
        H5Aclose(attr_id);
        H5Oclose(obj_id);
        return "";
    }
    pClassname[sDim] = 0;
    std::string className = std::string(pClassname);
    delete[] pClassname;
    H5Tclose(memtype);
    H5Sclose(aspace);
    H5Aclose(attr_id);
    H5Oclose(obj_id);
    return className;
}
//=============================================================================
Dimensions
getNelsonDimensions(hid_t fid, const std::string& location, const std::string& variableName)
{
    Dimensions res;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    hid_t obj_id = H5Oopen(fid, h5path.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return res;
    }
    hsize_t dims[2];
    hid_t attr_id
        = H5Aopen_by_name(obj_id, h5path.c_str(), NELSON_DIMENSIONS_STR, H5P_DEFAULT, H5P_DEFAULT);
    if (attr_id < 0) {
        H5Oclose(obj_id);
        return res;
    }
    hid_t aspace = H5Aget_space(attr_id);
    herr_t status = H5Sget_simple_extent_dims(aspace, dims, nullptr);
    if (status < 0) {
        H5Sclose(aspace);
        H5Oclose(obj_id);
        H5Aclose(attr_id);
        return res;
    }
    hid_t type = H5Aget_type(attr_id);
    uint64* ptrUint64 = nullptr;
    try {
        ptrUint64 = new uint64[(size_t)dims[1]];
    } catch (const std::bad_alloc&) {
        H5Tclose(type);
        H5Sclose(aspace);
        H5Oclose(obj_id);
        H5Aclose(attr_id);
        return res;
    }
    status = H5Aread(attr_id, type, ptrUint64);
    H5Tclose(type);
    H5Sclose(aspace);
    H5Oclose(obj_id);
    H5Aclose(attr_id);
    if (status < 0) {
        return res;
    }
    for (indexType k = 0; k < (indexType)dims[1]; k++) {
        res[k] = (indexType)ptrUint64[k];
    }
    delete[] ptrUint64;
    return res;
}
//=============================================================================
bool
isNelsonEmpty(hid_t fid, const std::string& location, const std::string& variableName)
{
    return attributeAsBool(fid, location, variableName, NELSON_EMPTY_STR);
}
//=============================================================================
bool
isNelsonSparse(hid_t fid, const std::string& location, const std::string& variableName)
{
    return attributeAsBool(fid, location, variableName, NELSON_SPARSE_STR);
}
//=============================================================================
bool
isNelsonObject(hid_t fid, const std::string& location, const std::string& variableName)
{
    return attributeAsBool(fid, location, variableName, NELSON_OBJECT_STR);
}
//=============================================================================
uint64
getNelsonNzmax(hid_t fid, const std::string& location, const std::string& variableName)
{
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    hid_t obj_id = H5Oopen(fid, h5path.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return 0;
    }
    htri_t exists = H5Aexists(obj_id, NELSON_SPARSE_NZMAX_STR);
    if (exists <= 0) {
        H5Oclose(obj_id);
        return 0;
    }
    hid_t attr_id = H5Aopen_by_name(
        obj_id, h5path.c_str(), NELSON_SPARSE_NZMAX_STR, H5P_DEFAULT, H5P_DEFAULT);
    if (attr_id < 0) {
        H5Oclose(obj_id);
        return 0;
    }
    uint64 value = 0;
    herr_t status = H5Aread(attr_id, H5T_NATIVE_UINT64, &value);
    if (status < 0) {
        H5Oclose(obj_id);
        H5Aclose(attr_id);
        return 0;
    }
    H5Aclose(attr_id);
    H5Oclose(obj_id);
    return value;
}
//=============================================================================
bool
isNelsonComplex(hid_t fid, const std::string& location, const std::string& variableName)
{
    return attributeAsBool(fid, location, variableName, NELSON_COMPLEX_STR);
}
//=============================================================================
hid_t
setCompression(const Dimensions& dims, bool useCompression)
{
    hid_t plist = H5I_INVALID_HID;
    if (dims.isEmpty(false) || dims.isScalar() || !useCompression) {
        return H5Pcopy(H5P_DEFAULT);
    }
    plist = H5Pcreate(H5P_DATASET_CREATE);
    if (H5Pset_layout(plist, H5D_COMPACT) < 0) {
        H5Pclose(plist);
        return H5Pcopy(H5P_DEFAULT);
    }
    hsize_t* dimsAsHsize_t = nullptr;
    indexType nbElementsSizeData = dims.length();
    try {
        dimsAsHsize_t = new_with_exception<hsize_t>(dims.length(), true);
    } catch (NelsonException&) {
        H5Pclose(plist);
        return H5Pcopy(H5P_DEFAULT);
    }
    Dimensions _dims = dims;
    for (indexType k = 1; k <= nbElementsSizeData; k++) {
        dimsAsHsize_t[k - 1] = (hsize_t)_dims[nbElementsSizeData - k];
    }
    herr_t status = H5Pset_chunk(plist, (int)dims.length(), dimsAsHsize_t);
    delete[] dimsAsHsize_t;
    if (status < 0) {
        H5Pclose(plist);
        plist = H5Pcopy(H5P_DEFAULT);
    } else {
        H5Pset_deflate(plist, NELSON_COMPRESSION_LEVEL);
    }
    return plist;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
