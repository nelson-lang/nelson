//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isfloatBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isfloatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bRes = (argIn[0].getDataClass() == NLS_DOUBLE || argIn[0].getDataClass() == NLS_DCOMPLEX
        || argIn[0].getDataClass() == NLS_SINGLE || argIn[0].getDataClass() == NLS_SCOMPLEX);
    retval << ArrayOf::logicalConstructor(bRes);
    return retval;
}
//=============================================================================
