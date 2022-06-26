//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "backgroundPool_usedBuiltin.hpp"
#include "BackgroundPoolObject.hpp"
#include "Error.hpp"
#include "usedHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::backgroundPool_usedBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    retval << usedHandle(BACKGROUNDPOOL_CATEGORY_STR);
    return retval;
}
//=============================================================================
