//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "streamVersionBuiltin.hpp"
#include "streamBuiltinHelpers.hpp"
#include "GzipCodec.hpp"
#include "ZstdCodec.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// v = __stream_version__(codec): library version, empty when the codec is not built.
ArrayOfVector
Nelson::FileArchiverGateway::streamVersionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << ArrayOf::stringArray(
        streamCodec(argIn[0]) == StreamCodec::Gzip ? GzipCodec::version() : ZstdCodec::version());
    return retval;
}
//=============================================================================
