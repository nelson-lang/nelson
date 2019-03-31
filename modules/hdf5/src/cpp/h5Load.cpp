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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/algorithm/string.hpp>
#include "h5Load.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "characters_encoding.hpp"
#include "h5LoadVariable.hpp"
#include "haveNh5Header.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5Load(Evaluator* eval, const std::wstring& filename, const wstringVector& names, bool asStruct)
{
    ArrayOf res;
    boost::filesystem::path hdf5_filename(filename);
    bool fileExistPreviously = false;
    try {
        fileExistPreviously = boost::filesystem::exists(hdf5_filename)
            && !boost::filesystem::is_directory(hdf5_filename);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            Error(_W("Permission denied."));
        }
        fileExistPreviously = false;
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
    }

    int16 nh5Version;
    int16 nh5Endian;
    std::wstring header;
    bool haveHeader = haveNh5Header(hdf5_filename.wstring(), header, nh5Version, nh5Endian);
    if (haveHeader) {
        if (nh5Version != NELSON_HEADER_VERSION) {
            Error(_W("Invalid file format."));
        }
    }
    hid_t fid
        = H5Fopen(wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (!haveHeader) {
        if (!isNelsonH5File(fid)) {
            H5Fclose(fid);
            Error(_W("Invalid file format."));
        }
    }
    stringVector variableNamesInFile = getVariableNames(fid);
    stringVector variableNames;
    if (names.empty()) {
        variableNames = variableNamesInFile;
    } else {
        for (std::wstring uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                != variableNamesInFile.end()) {
                variableNames.push_back(name);
            } else {
                std::string msg = _("Variable not found:") + std::string(" ") + name;
                Warning(msg);
            }
        }
    }
    ArrayOfVector values;
    for (std::string name : variableNames) {
        ArrayOf value;
        if (h5LoadVariable(fid, "/", name, value)) {
            values.push_back(value);
        } else {
            H5Fclose(fid);
            std::string msg = _("Cannot read variable:") + std::string(" ") + name;
            Error(msg);
        }
    }
    herr_t status = H5Fclose(fid);
    if (asStruct) {
        res = ArrayOf::structScalarConstructor(variableNames, values);
    } else {
        for (size_t i = 0; i < variableNames.size(); i++) {
            eval->getContext()->getCurrentScope()->insertVariable(variableNames[i], values[i]);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
