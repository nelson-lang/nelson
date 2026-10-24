//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NetcdfConstants.hpp"
#include <netcdf.h>
//=============================================================================
namespace Nelson::NetcdfGateway {
//=============================================================================
const std::map<std::string, int>&
netcdfConstants()
{
    static const std::map<std::string, int> constants = { { "NC_NOWRITE", NC_NOWRITE },
        { "NC_WRITE", NC_WRITE }, { "NC_CLOBBER", NC_CLOBBER }, { "NC_NOCLOBBER", NC_NOCLOBBER },
        { "NC_SHARE", NC_SHARE }, { "NC_64BIT_OFFSET", NC_64BIT_OFFSET },
        { "NC_NETCDF4", NC_NETCDF4 }, { "NC_CLASSIC_MODEL", NC_CLASSIC_MODEL },
        { "NC_64BIT_DATA", NC_64BIT_DATA }, { "NC_FORMAT_CLASSIC", NC_FORMAT_CLASSIC },
        { "NC_FORMAT_64BIT", NC_FORMAT_64BIT }, { "NC_FORMAT_NETCDF4", NC_FORMAT_NETCDF4 },
        { "NC_FORMAT_NETCDF4_CLASSIC", NC_FORMAT_NETCDF4_CLASSIC }, { "NC_GLOBAL", NC_GLOBAL },
        { "NC_UNLIMITED", static_cast<int>(NC_UNLIMITED) }, { "NC_NOFILL", NC_NOFILL },
        { "NC_FILL", NC_FILL }, { "NC_NAT", NC_NAT }, { "NC_BYTE", NC_BYTE },
        { "NC_CHAR", NC_CHAR }, { "NC_SHORT", NC_SHORT }, { "NC_INT", NC_INT },
        { "NC_FLOAT", NC_FLOAT }, { "NC_DOUBLE", NC_DOUBLE }, { "NC_UBYTE", NC_UBYTE },
        { "NC_USHORT", NC_USHORT }, { "NC_UINT", NC_UINT }, { "NC_INT64", NC_INT64 },
        { "NC_UINT64", NC_UINT64 }, { "NC_STRING", NC_STRING }, { "NC_CHUNKED", NC_CHUNKED },
        { "NC_CONTIGUOUS", NC_CONTIGUOUS }, { "NC_ENDIAN_NATIVE", NC_ENDIAN_NATIVE },
        { "NC_ENDIAN_LITTLE", NC_ENDIAN_LITTLE }, { "NC_ENDIAN_BIG", NC_ENDIAN_BIG } };
    return constants;
}
//=============================================================================
} // namespace Nelson::NetcdfGateway
//=============================================================================
