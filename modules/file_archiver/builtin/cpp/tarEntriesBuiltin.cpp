//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarEntriesBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// s = __tar_entries__(id): struct of column vectors describing every entry.
ArrayOfVector
Nelson::FileArchiverGateway::tarEntriesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::vector<TarEntryInfo> entries;
    std::string compression;
    std::string format;
    try {
        const TarArchive& archive = TarArchiveSessions::get(archiveId(argIn[0]));
        entries = archive.entries();
        compression = TarArchive::compressionName(archive.compression());
        format = archive.formatName();
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
    const indexType n = static_cast<indexType>(entries.size());
    wstringVector names;
    auto* sizes = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    auto* modified = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    auto* modes = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    auto* folders = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, n));
    auto* links = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, n));
    auto* encrypted = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, n));
    for (indexType k = 0; k < n; ++k) {
        const TarEntryInfo& entry = entries[k];
        names.push_back(utf8_to_wstring(entry.name));
        sizes[k] = static_cast<double>(entry.size);
        modified[k] = static_cast<double>(entry.modified);
        modes[k] = static_cast<double>(entry.mode);
        folders[k] = entry.isDirectory ? 1 : 0;
        links[k] = entry.isSymlink ? 1 : 0;
        encrypted[k] = entry.isEncrypted ? 1 : 0;
    }
    Dimensions column(n, 1);
    ArrayOfVector values;
    values << stringColumn(names);
    values << ArrayOf(NLS_DOUBLE, column, sizes);
    values << ArrayOf(NLS_DOUBLE, column, modified);
    values << ArrayOf(NLS_DOUBLE, column, modes);
    values << ArrayOf(NLS_LOGICAL, column, folders);
    values << ArrayOf(NLS_LOGICAL, column, links);
    values << ArrayOf::characterArray(compression);
    values << ArrayOf(NLS_LOGICAL, column, encrypted);
    values << ArrayOf::characterArray(format);
    const stringVector fields = { "Name", "Size", "Modified", "Mode", "IsFolder", "IsLink",
        "Compression", "Encrypted", "Format" };
    ArrayOfVector retval;
    retval << ArrayOf::structScalar(fields, values);
    return retval;
}
//=============================================================================
