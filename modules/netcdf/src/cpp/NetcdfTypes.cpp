//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NetcdfTypes.hpp"
#include <algorithm>
#include <cctype>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::NetcdfGateway {
//=============================================================================
nc_type
netcdfTypeFromName(const std::string& name)
{
    std::string type = netcdfLowerName(name);
    if (type == "double") {
        return NC_DOUBLE;
    }
    if (type == "single" || type == "float") {
        return NC_FLOAT;
    }
    if (type == "int8" || type == "byte") {
        return NC_BYTE;
    }
    if (type == "uint8") {
        return NC_UBYTE;
    }
    if (type == "int16") {
        return NC_SHORT;
    }
    if (type == "uint16") {
        return NC_USHORT;
    }
    if (type == "int32" || type == "int") {
        return NC_INT;
    }
    if (type == "uint32") {
        return NC_UINT;
    }
    if (type == "int64") {
        return NC_INT64;
    }
    if (type == "uint64") {
        return NC_UINT64;
    }
    if (type == "char" || type == "text") {
        return NC_CHAR;
    }
    Error(L"Nelson:netcdf:unsupportedDatatype", L"Unsupported netCDF datatype.");
    return NC_NAT;
}
//=============================================================================
nc_type
netcdfTypeFromArray(const ArrayOf& value)
{
    switch (value.dataClass()) {
    case NLS_SINGLE:
        return NC_FLOAT;
    case NLS_INT8:
        return NC_BYTE;
    case NLS_UINT8:
    case NLS_LOGICAL:
        return NC_UBYTE;
    case NLS_INT16:
        return NC_SHORT;
    case NLS_UINT16:
        return NC_USHORT;
    case NLS_INT32:
        return NC_INT;
    case NLS_UINT32:
        return NC_UINT;
    case NLS_INT64:
        return NC_INT64;
    case NLS_UINT64:
        return NC_UINT64;
    case NLS_CHAR:
        return NC_CHAR;
    default:
        return NC_DOUBLE;
    }
}
//=============================================================================
NelsonType
netcdfNelsonType(nc_type xtype)
{
    switch (xtype) {
    case NC_FLOAT:
        return NLS_SINGLE;
    case NC_BYTE:
        return NLS_INT8;
    case NC_UBYTE:
        return NLS_UINT8;
    case NC_SHORT:
        return NLS_INT16;
    case NC_USHORT:
        return NLS_UINT16;
    case NC_INT:
        return NLS_INT32;
    case NC_UINT:
        return NLS_UINT32;
    case NC_INT64:
        return NLS_INT64;
    case NC_UINT64:
        return NLS_UINT64;
    case NC_CHAR:
        return NLS_CHAR;
    default:
        return NLS_DOUBLE;
    }
}
//=============================================================================
std::string
netcdfTypeName(nc_type xtype)
{
    switch (xtype) {
    case NC_FLOAT:
        return "single";
    case NC_BYTE:
        return "int8";
    case NC_UBYTE:
        return "uint8";
    case NC_SHORT:
        return "int16";
    case NC_USHORT:
        return "uint16";
    case NC_INT:
        return "int32";
    case NC_UINT:
        return "uint32";
    case NC_INT64:
        return "int64";
    case NC_UINT64:
        return "uint64";
    case NC_CHAR:
        return "char";
    case NC_STRING:
        return "string";
    default:
        return "double";
    }
}
//=============================================================================
std::string
netcdfFormatName(int format)
{
    switch (format) {
    case NC_FORMAT_CLASSIC:
        return "classic";
    case NC_FORMAT_64BIT:
        return "64bit";
    case NC_FORMAT_NETCDF4:
        return "netcdf4";
    case NC_FORMAT_NETCDF4_CLASSIC:
        return "netcdf4_classic";
    default:
        return "unknown";
    }
}
//=============================================================================
std::string
netcdfLowerName(const std::string& value)
{
    std::string out = value;
    std::transform(out.begin(), out.end(), out.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return out;
}
//=============================================================================
} // namespace Nelson::NetcdfGateway
//=============================================================================
