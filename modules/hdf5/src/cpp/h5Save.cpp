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
#include <Eigen/Sparse>
#include "h5Save.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ClassName.hpp"
#include "SparseDynamicFunctions.hpp"
#include "h5SaveCell.hpp"
#include "h5SaveDouble.hpp"
#include "h5SaveHandle.hpp"
#include "h5SaveInteger.hpp"
#include "h5SaveLogical.hpp"
#include "h5SaveSingle.hpp"
#include "h5SaveString.hpp"
#include "h5SaveStruct.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
saveVariable(
    hid_t fid, const std::string& location, const std::string& variableName, ArrayOf VariableValue);
//=============================================================================
void
h5Save(Evaluator* eval, const std::wstring& filename, wstringVector names, bool append,
    bool nocompression)
{
    wstringVector variablesName;
    for (indexType k = 0; k < names.size(); k++) {

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
            if ((schema != NELSON_SCHEMA) || (schema == -1)) {
                Error(_W("Invalid file version."));
                H5Fclose(fid);
            }
        }
    }
    updateNelsonH5Header(fid);
    std::string location = "/";
    for (indexType k = 0; k < variablesName.size(); k++) {
        ArrayOf variableValue;
        std::string variableName = wstring_to_utf8(variablesName[k]);
        eval->getContext()->getCurrentScope()->lookupVariable(variableName, variableValue);
        bool bSuccess = saveVariable(fid, location, variableName, variableValue);
    }
    H5Fclose(fid);
}
//=============================================================================
bool
saveVariable(
    hid_t fid, const std::string& location, const std::string& variableName, ArrayOf VariableValue)
{
    bool bSuccess = false;
    switch (VariableValue.getDataClass()) {
    case NLS_HANDLE: {
        bSuccess = h5SaveHandle(fid, location, variableName, VariableValue);
    } break;
    case NLS_CELL_ARRAY: {
        bSuccess = h5SaveCell(fid, location, variableName, VariableValue);
    } break;
    case NLS_STRUCT_ARRAY: {
        bSuccess = h5SaveStruct(fid, location, variableName, VariableValue);
    } break;
    case NLS_STRING_ARRAY: {
        bSuccess = h5SaveStringArray(fid, location, variableName, VariableValue);
    } break;
    case NLS_LOGICAL: {
        bSuccess = h5SaveLogical(fid, location, variableName, VariableValue);
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        bSuccess = h5SaveInteger(fid, location, variableName, VariableValue);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        bSuccess = h5SaveSingle(fid, location, variableName, VariableValue);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        bSuccess = h5SaveDouble(fid, location, variableName, VariableValue);
    } break;
    case NLS_CHAR: {
        bSuccess = h5SaveCharacterArray(fid, location, variableName, VariableValue);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
