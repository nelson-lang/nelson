//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismatfileBuiltin.hpp"
#include "IsMatioFile.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Nelson::MatioGateway::ismatfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 3);
    nargincheck(argIn, 1, 1);
    wstringVector filenames = argIn[0].getContentAsWideStringVector(true);
    ArrayOf isMat;
    ArrayOf matVersions;
    ArrayOf matHeaders;
    IsMatioFile(filenames, isMat, matVersions, matHeaders);
    retval << isMat;
    if (nLhs > 1) {
        retval << matVersions;
    }
    if (nLhs > 2) {
        retval << matHeaders;
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
