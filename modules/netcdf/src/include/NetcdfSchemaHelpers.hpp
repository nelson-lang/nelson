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
#include <netcdf.h>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::NetcdfGateway {
ArrayOf
netcdfGroupInfo(int ncid);
ArrayOf
netcdfReadAtt(int ncid, int varid, const std::string& name);
ArrayOf
netcdfReadFill(int ncid, int varid, nc_type xtype, int& noFill);
void
netcdfWriteSchemaInto(int ncid, const ArrayOf& schema);
//=============================================================================
} // namespace Nelson::NetcdfGateway
//=============================================================================
