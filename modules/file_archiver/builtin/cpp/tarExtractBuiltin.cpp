//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarExtractBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "Tar.hpp"
#include "ioBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// filenames = __tar_extract__(id, outputFolder, maxExtractedSize, maxCompressionRatio)
ArrayOfVector
Nelson::FileArchiverGateway::tarExtractBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 1);
    const ExtractionLimits limits = extractionLimits(argIn[2], argIn[3]);
    wstringVector filenames;
    try {
        ExtractTar(TarArchiveSessions::get(archiveId(argIn[0])), argIn[1].asWideString(), filenames,
            limits);
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
    ArrayOfVector retval;
    retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(filenames);
    return retval;
}
//=============================================================================
