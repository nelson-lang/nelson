//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipFopenBuiltin.hpp"
#include "ArchiveFileBackends.hpp"
#include "ZipArchiveSessions.hpp"
#include "ioBuiltinHelpers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// fid = __zip_fopen__(id, name, displayName, mode, encoding, maxSize, compression, level,
// modified)
ArrayOfVector
Nelson::FileArchiverGateway::zipFopenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 9, 9);
    nargoutcheck(nLhs, 0, 1);
    const uint64_t id = archiveId(argIn[0]);
    const std::string name = argIn[1].asUtf8String();
    const std::wstring displayName = argIn[2].asWideString();
    const wchar_t mode = fileMode(argIn[3], false);
    const bool writing = mode == L'w';
    const std::wstring encoding = checkedEncoding(argIn[4]);
    FileBackend* backend = nullptr;
    try {
        if (writing) {
            ZipWriteOptions options = writeOptions(argIn[6], argIn[7]);
            options.modified = static_cast<time_t>(argIn[8].asDoubleScalar());
            ZipArchiveSessions::beginEntry(id, name, options);
            backend = new ZipEntryWriteBackend(id);
        } else {
            const uint64_t maxSize = sizeLimit(argIn[5]);
            const ZipArchive& archive = ZipArchiveSessions::get(id);
            const auto index = archive.find(name);
            if (!index) {
                throw ZipArchiveError(ZipErrorCode::EntryNotFound, "ZIP entry not found.", name);
            }
            backend = new ZipEntryReadBackend(archive.reopenForReading(), *index, maxSize);
        }
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
    ArrayOfVector retval;
    retval << openFileIdentifier(backend, displayName, mode, encoding);
    return retval;
}
//=============================================================================
