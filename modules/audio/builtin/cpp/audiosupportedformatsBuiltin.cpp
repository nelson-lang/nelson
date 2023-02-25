//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiosupportedformatsBuiltin.hpp"
#include "AudioSupportedFormats.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// info = audiosupportedformats()
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiosupportedformatsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 0);
    ArrayOfVector retval(1);
    retval << AudioSupportedFormats();
    return retval;
}
//=============================================================================
