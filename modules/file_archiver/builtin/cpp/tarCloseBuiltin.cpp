//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarCloseBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "zipBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// bytes = __tar_close__(id): bytes of an in-memory writer, empty otherwise.
// __tar_close__(id, true): closes once the entries written through file identifiers end.
ArrayOfVector
Nelson::FileArchiverGateway::tarCloseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::vector<uint8_t> bytes;
    try {
        if (argIn.size() == 2 && argIn[1].asLogicalScalar()) {
            TarArchiveSessions::closeWhenIdle(archiveId(argIn[0]));
        } else {
            bytes = TarArchiveSessions::close(archiveId(argIn[0]));
        }
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
    ArrayOfVector retval;
    retval << ArrayOf::uint8RowVector(std::vector<uint8>(bytes.begin(), bytes.end()));
    return retval;
}
//=============================================================================
