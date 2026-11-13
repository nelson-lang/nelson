//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarWriteBuiltin.hpp"
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
// __tar_write__(id, name, data, modified): modified is POSIX time, 0 for now.
ArrayOfVector
Nelson::FileArchiverGateway::tarWriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 0);
    try {
        TarArchiveSessions::addBuffer(archiveId(argIn[0]), argIn[1].asUtf8String(),
            byteVector(argIn[2]), static_cast<time_t>(argIn[3].asDoubleScalar()));
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
    return {};
}
//=============================================================================
