//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipWriteBuiltin.hpp"
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
// __zip_write__(id, name, bytes, compression, level, modified)
ArrayOfVector
Nelson::FileArchiverGateway::zipWriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 6, 6);
    nargoutcheck(nLhs, 0, 0);
    const std::string name = argIn[1].asUtf8String();
    ZipWriteOptions options = writeOptions(argIn[3], argIn[4]);
    options.modified = static_cast<time_t>(argIn[5].asDoubleScalar());
    const std::vector<uint8_t> bytes = byteVector(argIn[2]);
    try {
        ZipArchive& archive = ZipArchiveSessions::get(archiveId(argIn[0]));
        if (archive.find(name)) {
            Error(L"Nelson:io:zip:duplicateEntry", L"The archive already has an entry named: %s",
                utf8_to_wstring(name));
        }
        archive.addBuffer(name, bytes.data(), bytes.size(), options);
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
    return { };
}
//=============================================================================
