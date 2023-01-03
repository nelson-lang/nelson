//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "imreadBuiltin.hpp"
#include "ImageReader.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::imreadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval = {};
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 3);
    return imageReader(argIn[0].getContentAsWideString(), nLhs);
}
//=============================================================================
