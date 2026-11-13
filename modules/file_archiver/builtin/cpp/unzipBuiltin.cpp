//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "unzipBuiltin.hpp"
#include "Error.hpp"
#include "Unzip.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ioBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// filenames = __unzip__(zipfilename[, outputfolder[, maxExtractedSize, maxCompressionRatio]])
ArrayOfVector
Nelson::FileArchiverGateway::unzipBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    std::wstring zipFilename = argIn[0].asWideString();
    std::wstring rootPath = L".";
    if (argIn.size() > 1) {
        rootPath = argIn[1].asWideString();
    }
    ExtractionLimits limits;
    if (argIn.size() > 3) {
        limits = extractionLimits(argIn[2], argIn[3]);
    }
    wstringVector filenames;
    UnZip(zipFilename, rootPath, filenames, limits);
    if (nLhs > 0) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(filenames);
    }
    return retval;
}
//=============================================================================
