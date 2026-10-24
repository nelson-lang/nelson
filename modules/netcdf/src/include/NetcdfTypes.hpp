//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <netcdf.h>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::NetcdfGateway {
nc_type
netcdfTypeFromName(const std::string& name);
nc_type
netcdfTypeFromArray(const ArrayOf& value);
NelsonType
netcdfNelsonType(nc_type xtype);
std::string
netcdfTypeName(nc_type xtype);
std::string
netcdfFormatName(int format);
std::string
netcdfLowerName(const std::string& value);
//=============================================================================
} // namespace Nelson::NetcdfGateway
//=============================================================================
