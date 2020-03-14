//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <matio.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/algorithm/string.hpp>
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
        versions = ArrayOf::stringArrayConstructor(filenames, dims);
        headers = ArrayOf::stringArrayConstructor(filenames, dims);
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
        boost::filesystem::path mat_filename(filename);
        bool fileExistPreviously = false;
        try {
            fileExistPreviously = boost::filesystem::exists(mat_filename)
                && !boost::filesystem::is_directory(mat_filename);
        } catch (const boost::filesystem::filesystem_error&) {
            fileExistPreviously = false;
        }
        if (!fileExistPreviously) {
            res[k] = false;
            ArrayOf empty = ArrayOf::emptyConstructor(0, 1);
            empty.promoteType(NLS_CHAR);
            elementVersions[k] = ArrayOf::characterArrayConstructor("");
            elementHeaders[k] = ArrayOf::characterArrayConstructor("");
        } else {
            std::string utf8filename = wstring_to_utf8(filename);
            mat_t* matfile = Mat_Open(utf8filename.c_str(), MAT_ACC_RDONLY);
            if (matfile) {
#if MATIO_VERSION >= 1515
                const char* headermat = Mat_GetHeader(matfile);
#else
                const char* headermat = nullptr;
#endif
                if (headermat) {
                    std::string headertrimleft = std::string(headermat);
                    boost::algorithm::trim_right(headertrimleft);
                    elementHeaders[k] = ArrayOf::characterArrayConstructor(headertrimleft);
                } else {
                    elementHeaders[k] = ArrayOf::characterArrayConstructor("");
                }
                mat_ft matVer = Mat_GetVersion(matfile);
                switch (matVer) {
                case MAT_FT_MAT73: {
                    res[k] = true;
                    elementVersions[k] = ArrayOf::characterArrayConstructor("-v7.3");
                } break;
                case MAT_FT_MAT5: {
                    res[k] = true;
                    elementVersions[k] = ArrayOf::characterArrayConstructor("-v7");
                } break;
                case MAT_FT_MAT4: {
                    res[k] = true;
                    elementVersions[k] = ArrayOf::characterArrayConstructor("-v6");
                } break;
                default: {
                    res[k] = false;
                    elementVersions[k] = ArrayOf::characterArrayConstructor("");
                } break;
                }
                Mat_Close(matfile);
            } else {
                res[k] = false;
                elementVersions[k] = ArrayOf::characterArrayConstructor("");
                elementHeaders[k] = ArrayOf::characterArrayConstructor("");
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
