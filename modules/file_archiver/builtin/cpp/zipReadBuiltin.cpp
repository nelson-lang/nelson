//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipReadBuiltin.hpp"
#include "zipBuiltinHelpers.hpp"
#include "ioBuiltinHelpers.hpp"
#include "ZipArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// bytes = __zip_read__(id, name, maxSize)
ArrayOfVector
Nelson::FileArchiverGateway::zipReadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    const std::string name = argIn[1].asUtf8String();
    const uint64_t maxSize = sizeLimit(argIn[2]);
    std::vector<uint8> bytes;
    try {
        ZipArchive& archive = ZipArchiveSessions::get(archiveId(argIn[0]));
        auto index = archive.find(name);
        if (!index) {
            throw ZipArchiveError(ZipErrorCode::EntryNotFound, "ZIP entry not found.", name);
        }
        std::vector<uint8_t> content = archive.readEntry(*index, maxSize);
        bytes.assign(content.begin(), content.end());
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
    ArrayOfVector retval;
    retval << ArrayOf::uint8RowVector(bytes);
    return retval;
}
//=============================================================================
