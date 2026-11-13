//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarOpenBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
#include "zipBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// id = __tar_open__(source, mode, compression, level): file name, uint8 bytes ('r') or []
// for a new in-memory archive; compression 'auto' follows the file name when writing.
ArrayOfVector
Nelson::FileArchiverGateway::tarOpenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring mode = argIn[1].asWideString();
    if (mode != L"r" && mode != L"w") {
        Error(L"Nelson:io:tar:invalidMode", L"Mode must be 'r' or 'w'.");
    }
    const bool isFile = argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray();
    const std::wstring source = isFile ? argIn[0].asWideString() : L"<bytes>";
    uint64_t id = 0;
    try {
        if (mode == L"r") {
            if (isFile) {
                id = TarArchiveSessions::openFile(source);
            } else if (argIn[0].dataClass() == NLS_UINT8) {
                id = TarArchiveSessions::openMemory(byteVector(argIn[0]));
            } else {
                Error(L"Nelson:io:tar:invalidSource",
                    L"Source must be a file name, uint8 data to read, or [] to write in memory.");
            }
        } else {
            if (!isFile && !(argIn[0].isEmpty() && argIn[0].isNumeric())) {
                Error(L"Nelson:io:tar:invalidSource",
                    L"Source must be a file name, uint8 data to read, or [] to write in memory.");
            }
            const std::string name = argIn[2].asUtf8String();
            TarCompression compression = TarCompression::None;
            if (name == "auto") {
                compression
                    = isFile ? TarArchive::compressionFromFilename(source) : TarCompression::None;
            } else if (!TarArchive::compressionFromName(name, compression)) {
                Error(L"Nelson:io:tar:unknownCompression",
                    L"Compression must be 'auto', 'none', 'gzip', 'zstd', 'xz', 'bzip2', 'lz4' or "
                    L"'lzma'.");
            }
            const double level = argIn[3].asDoubleScalar();
            int minimum = 0;
            int maximum = 0;
            if (level != -1 && TarArchive::levelRange(compression, minimum, maximum)
                && (level != static_cast<int>(level) || level < minimum || level > maximum)) {
                Error(L"Nelson:io:tar:invalidLevel", L"Level must be an integer from %d to %d.",
                    minimum, maximum);
            }
            id = TarArchiveSessions::create(isFile ? source : L"", compression,
                static_cast<int>(level),
                static_cast<int>(NelsonConfiguration::getInstance()->maxNumCompThreads()));
        }
    } catch (const ZipArchiveError& error) {
        raiseTarError(error, source);
    }
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar(static_cast<double>(id));
    return retval;
}
//=============================================================================
