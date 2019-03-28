//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <matio.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "IsMatioFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
IsMatioFile(const wstringVector &filenames, ArrayOf& results, ArrayOf& versions)
{
    Dimensions dims(filenames.size(), 1);
    logical* res = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, filenames.size());
    results = ArrayOf(NLS_LOGICAL, dims, res);

    if (filenames.size() == 0) {
        versions = ArrayOf::stringArrayConstructor(filenames, dims);
        return;
    }
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, filenames.size());
    versions = ArrayOf(NLS_STRING_ARRAY, dims, elements);

    for (size_t k = 0; k < filenames.size(); ++k) {
        std::wstring filename = filenames[k];
        boost::filesystem::path mat_filename(filename);
        bool fileExistPreviously = false;
        try {
            fileExistPreviously = boost::filesystem::exists(mat_filename)
                && !boost::filesystem::is_directory(mat_filename);
        } catch (const boost::filesystem::filesystem_error& e) {
            fileExistPreviously = false;
        }
        if (!fileExistPreviously) {
            res[k] = false;
            elements[k] = ArrayOf::characterArrayConstructor("");
        } else {
            std::string utf8filename = wstring_to_utf8(filename);
            mat_t* matfile = Mat_Open(utf8filename.c_str(), MAT_ACC_RDONLY);
            if (matfile) {
                mat_ft matVer = Mat_GetVersion(matfile);
                switch (matVer) {
                case MAT_FT_MAT73: {
                    res[k] = true;
                    elements[k] = ArrayOf::characterArrayConstructor("-v7.3");
                } break;
                case MAT_FT_MAT5: {
                    res[k] = true;
                    elements[k] = ArrayOf::characterArrayConstructor("-v7");
                } break;
                case MAT_FT_MAT4: {
                    res[k] = true;
                    elements[k] = ArrayOf::characterArrayConstructor("-v6");
                } break;
                default: {
                    res[k] = false;
                    elements[k] = ArrayOf::characterArrayConstructor("");
                } break;
                }
                Mat_Close(matfile);
            } else {
                res[k] = false;
                elements[k] = ArrayOf::characterArrayConstructor("");
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
