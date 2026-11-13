//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipEntriesBuiltin.hpp"
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
// s = __zip_entries__(id): struct of column vectors describing every entry.
ArrayOfVector
Nelson::FileArchiverGateway::zipEntriesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::vector<ZipEntryInfo> entries;
    try {
        entries = ZipArchiveSessions::get(archiveId(argIn[0])).entries();
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
    const indexType n = static_cast<indexType>(entries.size());
    wstringVector names;
    wstringVector methods;
    auto* sizes = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    auto* compressed = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    auto* modified = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    auto* folders = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, n));
    auto* crcs = static_cast<uint32*>(ArrayOf::allocateArrayOf(NLS_UINT32, n));
    auto* encrypted = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, n));
    for (indexType k = 0; k < n; ++k) {
        const ZipEntryInfo& entry = entries[k];
        names.push_back(utf8_to_wstring(entry.name));
        methods.push_back(utf8_to_wstring(ZipArchive::compressionName(entry.method)));
        sizes[k] = static_cast<double>(entry.size);
        compressed[k] = static_cast<double>(entry.compressedSize);
        modified[k] = static_cast<double>(entry.modified);
        folders[k] = entry.isDirectory ? 1 : 0;
        crcs[k] = entry.crc32;
        encrypted[k] = entry.isEncrypted ? 1 : 0;
    }
    Dimensions column(n, 1);
    ArrayOfVector values;
    values << stringColumn(names);
    values << ArrayOf(NLS_DOUBLE, column, sizes);
    values << ArrayOf(NLS_DOUBLE, column, compressed);
    values << stringColumn(methods);
    values << ArrayOf(NLS_DOUBLE, column, modified);
    values << ArrayOf(NLS_LOGICAL, column, folders);
    values << ArrayOf(NLS_UINT32, column, crcs);
    values << ArrayOf(NLS_LOGICAL, column, encrypted);
    const stringVector fields = { "Name", "Size", "CompressedSize", "Compression", "Modified",
        "IsFolder", "CRC32", "Encrypted" };
    ArrayOfVector retval;
    retval << ArrayOf::structScalar(fields, values);
    return retval;
}
//=============================================================================
