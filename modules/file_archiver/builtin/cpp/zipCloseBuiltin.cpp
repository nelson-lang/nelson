//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipCloseBuiltin.hpp"
#include "zipBuiltinHelpers.hpp"
#include "ZipArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// bytes = __zip_close__(id): bytes of an in-memory writer, empty otherwise.
// __zip_close__(id, true): closes once the entry written through a file identifier ends.
ArrayOfVector
Nelson::FileArchiverGateway::zipCloseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::vector<uint8_t> bytes;
    try {
        if (argIn.size() == 2 && argIn[1].asLogicalScalar()) {
            ZipArchiveSessions::closeWhenIdle(archiveId(argIn[0]));
        } else {
            bytes = ZipArchiveSessions::close(archiveId(argIn[0]));
        }
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
    ArrayOfVector retval;
    retval << ArrayOf::uint8RowVector(std::vector<uint8>(bytes.begin(), bytes.end()));
    return retval;
}
//=============================================================================
