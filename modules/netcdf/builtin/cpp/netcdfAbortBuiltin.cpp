//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "netcdfAbortBuiltin.hpp"
#include "netcdfBuiltin.hpp"
//=============================================================================
namespace Nelson::NetcdfGateway {
//=============================================================================
ArrayOfVector
netcdfAbortBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector args;
    args.reserve(argIn.size() + 1);
    args << ArrayOf::characterArray(L"netcdf.abort");
    for (const auto& arg : argIn) {
        args << arg;
    }
    return netcdfBuiltin(eval, nLhs, args);
}
//=============================================================================
} // namespace Nelson::NetcdfGateway
//=============================================================================
