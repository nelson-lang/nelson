//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarFopenBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "ioBuiltinHelpers.hpp"
#include "ArchiveFileBackends.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// fid = __tar_fopen__(id, name, displayName, mode, encoding, maxSize)
ArrayOfVector
Nelson::FileArchiverGateway::tarFopenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 6, 6);
    nargoutcheck(nLhs, 0, 1);
    const uint64_t id = archiveId(argIn[0]);
    const std::string name = argIn[1].asUtf8String();
    const std::wstring displayName = argIn[2].asWideString();
    const wchar_t mode = fileMode(argIn[3], false);
    const std::wstring encoding = checkedEncoding(argIn[4]);
    FileBackend* backend = nullptr;
    try {
        if (mode == L'w') {
            TarArchiveSessions::beginEntry(id, name);
            backend = new TarEntryWriteBackend(id, name);
        } else {
            const uint64_t maxSize = sizeLimit(argIn[5]);
            const TarArchive& archive = TarArchiveSessions::get(id);
            const auto index = archive.find(name);
            if (!index) {
                throw ZipArchiveError(ZipErrorCode::EntryNotFound, "tar entry not found.", name);
            }
            backend = new TarEntryReadBackend(archive.reopenForReading(), *index, maxSize);
        }
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
    ArrayOfVector retval;
    retval << openFileIdentifier(backend, displayName, mode, encoding);
    return retval;
}
//=============================================================================
