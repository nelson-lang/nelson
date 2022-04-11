//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "unzipBuiltin.hpp"
#include "Error.hpp"
#include "Unzip.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FileArchiverGateway::unzipBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::wstring zipFilename = argIn[0].getContentAsWideString();
    std::wstring rootPath = L".";
    if (argIn.size() > 1) {
        rootPath = argIn[1].getContentAsWideString();
    }
    wstringVector filenames;

    UnZip(zipFilename, rootPath, filenames);
    if (nLhs > 0) {
        retval << ToCellStringAsRow(filenames);
    }
    return retval;
}
//=============================================================================
