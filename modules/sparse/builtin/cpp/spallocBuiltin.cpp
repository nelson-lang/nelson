//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "spallocBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SparseConstructors.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::spallocBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);

    indexType m = argIn[0].asScalarIndex();
    indexType n = argIn[1].asScalarIndex();
    indexType nz = argIn[2].asScalarIndex(true);
    ArrayOf S = SparseConstructor(m, n);
    S.setSparseNzmax(nz);

    ArrayOfVector retval;
    retval << S;
    return retval;
}
//=============================================================================
