//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "streamTobytesBuiltin.hpp"
#include "ArchiveFileBackends.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileClose.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// bytes = __stream_tobytes__(fid): finishes and closes an in-memory writer.
ArrayOfVector
Nelson::FileArchiverGateway::streamTobytesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    const auto fid = static_cast<int>(argIn[0].asDoubleScalar());
    File* file = (fm != nullptr && fm->isOpened(fid)) ? fm->getFile(fid) : nullptr;
    auto* writer
        = file != nullptr ? dynamic_cast<CompressedWriteBackend*>(file->getBackend()) : nullptr;
    if (writer == nullptr || !writer->isInMemory()) {
        Error(L"Nelson:io:notMemoryWriter",
            L"The file identifier must be an in-memory writer opened with fopen([], 'w').");
    }
    writer->close();
    std::vector<uint8> bytes(writer->memoryBytes().begin(), writer->memoryBytes().end());
    FileClose(fm, fid);
    ArrayOfVector retval;
    retval << ArrayOf::uint8RowVector(bytes);
    return retval;
}
//=============================================================================
