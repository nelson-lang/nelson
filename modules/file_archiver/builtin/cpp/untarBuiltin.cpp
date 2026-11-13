//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "untarBuiltin.hpp"
#include "Tar.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ioBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// filenames = __untar__(tarfilename[, outputfolder[, maxExtractedSize, maxCompressionRatio]])
ArrayOfVector
Nelson::FileArchiverGateway::untarBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring tarFilename = argIn[0].asWideString();
    const std::wstring rootPath = argIn.size() > 1 ? argIn[1].asWideString() : L".";
    ExtractionLimits limits;
    if (argIn.size() > 3) {
        limits = extractionLimits(argIn[2], argIn[3]);
    }
    wstringVector filenames;
    UnTar(tarFilename, rootPath, filenames, limits);
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(filenames);
    }
    return retval;
}
//=============================================================================
