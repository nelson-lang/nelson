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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/container/vector.hpp>
#include "whoNh5File.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5LoadVariable.hpp"
#include "characters_encoding.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
whoNh5File(Interface* io, const std::wstring& filename, const wstringVector& names, bool asCell)
{
    ArrayOf res;
    boost::filesystem::path nh5_filename(filename);
    bool fileExistPreviously = false;
    try {
        fileExistPreviously = boost::filesystem::exists(nh5_filename)
            && !boost::filesystem::is_directory(nh5_filename);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            Error(_W("Permission denied."));
        }
        fileExistPreviously = false;
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
    }

    hid_t fid
        = H5Fopen(wstring_to_utf8(nh5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (!isNelsonH5File(fid)) {
        H5Fclose(fid);
        Error(_W("Invalid file format."));
    }
    stringVector variableNamesInFile = getVariableNames(fid);
    H5Fclose(fid);
    stringVector variablesNamesToRead;
    if (names.empty()) {
        variablesNamesToRead = variableNamesInFile;
    } else {
        for (std::wstring uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                != variableNamesInFile.end()) {
                variablesNamesToRead.push_back(name);
            }
        }
    }
    if (asCell) {
        Dimensions dims(variablesNamesToRead.size(), 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
        indexType k = 0;
        for (std::string name : variablesNamesToRead) {
            elements[k] = ArrayOf::characterArrayConstructor(name);
            k++;
        }
        res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        if (variablesNamesToRead.size()) {
            size_t ncharmax = io->getTerminalWidth();
            size_t nbchar = 0;
            io->outputMessage(_W("Your variables are:") + L"\n\n");
            for (auto& k : variablesNamesToRead) {
                if (nbchar + k.size() < ncharmax) {
                    io->outputMessage(k);
                    io->outputMessage(" ");
                    nbchar = 1 + nbchar + k.size();
                } else {
                    nbchar = 0;
                    io->outputMessage("\n");
                    io->outputMessage(k);
                    io->outputMessage(" ");
                    nbchar = 1 + nbchar + k.size();
                }
            }
            io->outputMessage("\n");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
