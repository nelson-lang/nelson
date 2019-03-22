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
#include "h5Save.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ClassName.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
h5Save(Evaluator* eval, const std::wstring& filename, wstringVector names, bool append,
    bool nocompression)
{
    wstringVector variablesName;
    for (size_t k = 0; k < names.size(); k++) {

        if (!IsValidVariableName(names[k])) {
            Error(_W("Invalid variable name:") + names[k]);
        }
        if (!eval->getContext()->isVariable(names[k])) {
            Error(_W("Variable does not exist:") + names[k]);
        }
    }

    variablesName = names;
    if (variablesName.empty()) {
        eval->getContext()->getCurrentScope()->getVariablesList(false, variablesName);
    }

    hid_t fid = H5I_INVALID_HID;
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
        fid = H5Fcreate(wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_TRUNC,
            H5P_DEFAULT, H5P_DEFAULT);
    } else {
        if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str()))
            Error(_W("HDF5 format file expected."));
        else {
            if (append) {
                fid = H5Fopen(
                    wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
            } else {
                try {
                    boost::filesystem::path p = hdf5_filename;
                    boost::filesystem::remove(p);
                } catch (const boost::filesystem::filesystem_error& e) {
                    Error(_W("Cannot replace file"));
                    boost::system::error_code error_code = e.code();
                }
                fid = H5Fcreate(wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_TRUNC,
                    H5P_DEFAULT, H5P_DEFAULT);
                updateNelsonH5Header(fid);
            }
        }
    }

    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (fileExistPreviously) {
        if (!isNelsonH5File(fid)) {
            H5Fclose(fid);
            Error(_W("Invalid file format."));
        }
        if (append) {
            int32 schema = getNelsonH5Schema(fid);
            bool isUnmanaged = (schema != NELSON_SCHEMA);
            if (isUnmanaged) {
                Error(_W("Invalid file version."));
                H5Fclose(fid);
            }
        }
    }
    if (!fileExistPreviously) {
        updateNelsonH5Header(fid);
    }

    std::string location = "/";
    for (size_t k = 0; k < variablesName.size(); k++) {
        ArrayOf variableValue;
        std::string variableName = wstring_to_utf8(variablesName[k]);
        eval->getContext()->getCurrentScope()->lookupVariable(variableName, variableValue);
        bool bSuccess = h5SaveVariable(fid, location, variableName, variableValue, !nocompression);
        if (!bSuccess) {
            H5Fclose(fid);
            Error(_("Cannot save variable:") + variableName);
        }
    }
    H5Fclose(fid);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
