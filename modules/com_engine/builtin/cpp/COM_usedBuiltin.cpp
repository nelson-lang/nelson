//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "COM_usedBuiltin.hpp"
#include "ComHandleObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "usedHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_usedBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    retval << usedHandle(NLS_HANDLE_COM_CATEGORY_STR);
    return retval;
}
//=============================================================================
