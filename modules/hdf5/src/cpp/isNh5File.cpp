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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "isNh5File.hpp"
#include "characters_encoding.hpp"
#include "h5SaveLoadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
isNh5File(wstringVector filenames)
{
    Dimensions dims(filenames.size(), 1);
    logical* res = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, filenames.size());
    ArrayOf results = ArrayOf(NLS_LOGICAL, dims, res);
    if (filenames.size() == 0) {
        return results;
    }
    for (size_t k = 0; k < filenames.size(); ++k) {
        std::wstring filename = filenames[k];
        boost::filesystem::path mat_filename(filename);
        bool fileExistPreviously = false;
        try {
            fileExistPreviously = boost::filesystem::exists(mat_filename)
                && !boost::filesystem::is_directory(mat_filename);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
            }
            fileExistPreviously = false;
        }
        if (!fileExistPreviously) {
            res[k] = false;
        } else {
            std::string utf8filename = wstring_to_utf8(filename);
            hid_t fid = H5Fopen(utf8filename.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
            if (fid == H5I_INVALID_HID) {
                res[k] = false;
            } else {
                res[k] = isNelsonH5File(fid);
                H5Fclose(fid);
            }
        }
    }
    return results;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
