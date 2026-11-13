//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarCompressionsBuiltin.hpp"
#include "zipBuiltinHelpers.hpp"
#include "TarArchive.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// names = __tar_compressions__()
ArrayOfVector
Nelson::FileArchiverGateway::tarCompressionsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    wstringVector names;
    for (TarCompression method : TarArchive::availableCompressions()) {
        names.push_back(utf8_to_wstring(TarArchive::compressionName(method)));
    }
    ArrayOfVector retval;
    Dimensions row(1, names.size());
    retval << ArrayOf::stringArray(names, row);
    return retval;
}
//=============================================================================
