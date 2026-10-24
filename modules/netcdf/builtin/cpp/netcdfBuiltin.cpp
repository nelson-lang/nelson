//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NetcdfDispatcher.hpp"
#include "netcdfBuiltin.hpp"
//=============================================================================
namespace Nelson::NetcdfGateway {
//=============================================================================
ArrayOfVector
netcdfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return netcdfDispatcher(eval, nLhs, argIn);
}
//=============================================================================
} // namespace Nelson::NetcdfGateway
//=============================================================================
