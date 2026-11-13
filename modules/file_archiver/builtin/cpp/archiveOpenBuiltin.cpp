//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "archiveOpenBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// id = __archive_open__(source, password): read-only archive of any format libarchive
// reads (file name or uint8 bytes); the entries use the __tar_*__ builtins.
ArrayOfVector
Nelson::FileArchiverGateway::archiveOpenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    const bool isFile = argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray();
    if (!isFile && argIn[0].dataClass() != NLS_UINT8) {
        Error(L"Nelson:io:archive:invalidSource", L"Source must be a file name or uint8 data.");
    }
    const std::wstring source = isFile ? argIn[0].asWideString() : L"<bytes>";
    TarReadOptions options;
    options.anyFormat = true;
    options.password = argIn[1].asUtf8String();
    uint64_t id = 0;
    try {
        id = isFile ? TarArchiveSessions::openFile(source, options)
                    : TarArchiveSessions::openMemory(byteVector(argIn[0]), options);
    } catch (const ZipArchiveError& error) {
        if (error.code() == ZipErrorCode::NotAZipArchive) {
            Error(L"Nelson:io:archive:unknownFormat", L"Not a supported archive: %s", source);
        }
        raiseTarError(error, source);
    }
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar(static_cast<double>(id));
    return retval;
}
//=============================================================================
