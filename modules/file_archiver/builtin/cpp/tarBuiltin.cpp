//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarBuiltin.hpp"
#include "Tar.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// entrynames = tar(tarfilename, files[, rootfolder])
ArrayOfVector
Nelson::FileArchiverGateway::tarBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    std::wstring tarFilename = argIn[0].asWideString();
    const wstringVector names = argIn[1].asWideStringVector();
    std::wstring rootPath = L".";
    if (argIn.size() > 2) {
        rootPath = argIn[2].asWideString();
        if (rootPath.empty()) {
            rootPath = L".";
        }
    }
    // Like zip, ".tar" is added only when the name has no extension.
    const size_t nameStart = tarFilename.find_last_of(L"/\\");
    const size_t dot = tarFilename.find_last_of(L'.');
    if (dot == std::wstring::npos || (nameStart != std::wstring::npos && dot < nameStart)) {
        tarFilename += L".tar";
    }
    wstringVector entrynames;
    Tar(tarFilename, names, rootPath, entrynames);
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(entrynames);
    }
    return retval;
}
//=============================================================================
