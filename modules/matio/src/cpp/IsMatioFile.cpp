//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <matio.h>
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "IsMatioFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
IsMatioFile(const wstringVector& filenames, ArrayOf& results, ArrayOf& versions, ArrayOf& headers)
{
    Dimensions dims(filenames.size(), 1);
    logical* res
        = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, filenames.size(), stringVector(), false);
    results = ArrayOf(NLS_LOGICAL, dims, res);

    if (filenames.size() == 0) {
        versions = ArrayOf::stringArray(filenames, dims);
        headers = ArrayOf::stringArray(filenames, dims);
        return;
    }
    ArrayOf* elementVersions = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, filenames.size(), stringVector(), false);
    versions = ArrayOf(NLS_STRING_ARRAY, dims, elementVersions);

    ArrayOf* elementHeaders = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, filenames.size(), stringVector(), false);
    headers = ArrayOf(NLS_STRING_ARRAY, dims, elementHeaders);

    for (size_t k = 0; k < filenames.size(); ++k) {
        std::wstring filename = filenames[k];
        FileSystemWrapper::Path mat_filename(filename);
        bool fileExistPreviously = FileSystemWrapper::Path::is_regular_file(mat_filename);
        if (!fileExistPreviously) {
            res[k] = false;
            ArrayOf empty = ArrayOf::empty(0, 1);
            empty.promoteType(NLS_CHAR);
            elementVersions[k] = ArrayOf::characterArray("");
            elementHeaders[k] = ArrayOf::characterArray("");
        } else {
            std::string utf8filename = wstring_to_utf8(filename);
            mat_t* matfile = Mat_Open(utf8filename.c_str(), MAT_ACC_RDONLY);
            if (matfile) {
#if MATIO_VERSION > 1515
                const char* headermat = Mat_GetHeader(matfile);
#else
                const char* headermat = nullptr;
#endif
                if (headermat) {
                    std::string headertrimleft = std::string(headermat);
                    StringHelpers::trim_right(headertrimleft);
                    elementHeaders[k] = ArrayOf::characterArray(headertrimleft);
                } else {
                    elementHeaders[k] = ArrayOf::characterArray("");
                }
                mat_ft matVer = Mat_GetVersion(matfile);
                switch (matVer) {
                case MAT_FT_MAT73: {
                    res[k] = true;
                    elementVersions[k] = ArrayOf::characterArray("-v7.3");
                } break;
                case MAT_FT_MAT5: {
                    res[k] = true;
                    elementVersions[k] = ArrayOf::characterArray("-v7");
                } break;
                case MAT_FT_MAT4: {
                    res[k] = true;
                    elementVersions[k] = ArrayOf::characterArray("-v6");
                } break;
                default: {
                    res[k] = false;
                    elementVersions[k] = ArrayOf::characterArray("");
                } break;
                }
                Mat_Close(matfile);
            } else {
                res[k] = false;
                elementVersions[k] = ArrayOf::characterArray("");
                elementHeaders[k] = ArrayOf::characterArray("");
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
