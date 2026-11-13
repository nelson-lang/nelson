//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarReadBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "ioBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// data = __tar_read__(id, name, maxSize)
ArrayOfVector
Nelson::FileArchiverGateway::tarReadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    const std::string name = argIn[1].asUtf8String();
    const uint64_t maxSize = sizeLimit(argIn[2]);
    std::vector<uint8_t> data;
    try {
        TarArchive& archive = TarArchiveSessions::get(archiveId(argIn[0]));
        const auto index = archive.find(name);
        if (!index) {
            throw ZipArchiveError(ZipErrorCode::EntryNotFound, "tar entry not found.", name);
        }
        data = archive.readEntry(*index, maxSize);
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
    ArrayOfVector retval;
    retval << ArrayOf::uint8RowVector(std::vector<uint8>(data.begin(), data.end()));
    return retval;
}
//=============================================================================
