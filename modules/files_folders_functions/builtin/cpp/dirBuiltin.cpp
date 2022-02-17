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
#include <boost/container/vector.hpp>
#include "dirBuiltin.hpp"
#include "Error.hpp"
#include "GetCurrentDirectory.hpp"
#include "ListFiles.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonPrint.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::dirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    std::wstring wpath;
    std::wstring woption;
    bool bSubDirectories = false;
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 0: {
        wpath = GetCurrentDirectory();
        if (wpath.empty()) {
            Error(_W("Impossible to get current directory."));
        }
    } break;
    case 2: {
        if (!argIn[1].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        woption = argIn[1].getContentAsWideString();
        if (woption == L"-s") {
            bSubDirectories = true;
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_VALUE);
        }
    }
    case 1: {
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        wpath = argIn[0].getContentAsWideString();
    } break;
    default:
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        break;
    }
    boost::container::vector<FileInfo> res = ListFiles(wpath, bSubDirectories);
    if (nLhs == 0) {
        if (res.empty()) {
            std::wstring msg = std::wstring(L"\'") + wpath + std::wstring(L"\' ")
                + _W("Not a file or a directory.");
            NelsonPrint(msg);
        } else {
            NelsonPrint("\n");
            for (auto& re : res) {
                if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                    break;
                }
                std::wstring filename;
                if (bSubDirectories) {
                    filename = re.getFolder() + L"/" + re.getName();
                } else {
                    filename = re.getName();
                }
                if (re.isDir()) {
                    if (re.getName() != L"." && re.getName() != L"..") {
                        filename = filename + L"/";
                    }
                }
                NelsonPrint(filename + L"\n");
            }
            NelsonPrint("\n");
        }
    } else {
        stringVector fieldnames;
        fieldnames.push_back("name");
        fieldnames.push_back("folder");
        fieldnames.push_back("date");
        fieldnames.push_back("bytes");
        fieldnames.push_back("isdir");
        fieldnames.push_back("datenum");
        Dimensions dims;
        dims[0] = res.size();
        dims[1] = 1;
        if (res.empty()) {
            retval << ArrayOf::emptyStructConstructor(fieldnames, dims);
        } else {
            auto* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
            ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
            ArrayOfVector names;
            ArrayOfVector folder;
            ArrayOfVector dates;
            ArrayOfVector bytes;
            ArrayOfVector isdirs;
            ArrayOfVector datenums;
            names.reserve(res.size());
            folder.reserve(res.size());
            dates.reserve(res.size());
            bytes.reserve(res.size());
            isdirs.reserve(res.size());
            datenums.reserve(res.size());
            for (auto& re : res) {
                names.push_back(ArrayOf::characterArrayConstructor(re.getName()));
                folder.push_back(ArrayOf::characterArrayConstructor(re.getFolder()));
                dates.push_back(ArrayOf::characterArrayConstructor(re.getDate()));
                double bytesval = re.getBytes();
                if (bytesval == -1) {
                    bytes.push_back(ArrayOf::emptyConstructor());
                } else {
                    bytes.push_back(ArrayOf::doubleConstructor(bytesval));
                }
                isdirs.push_back(ArrayOf::logicalConstructor(re.isDir()));
                double datenumval = re.getDatenum();
                if (datenumval == -1) {
                    datenums.push_back(ArrayOf::emptyConstructor());
                } else {
                    datenums.push_back(ArrayOf::doubleConstructor(datenumval));
                }
            }
            st.setFieldAsList("name", names);
            st.setFieldAsList("folder", folder);
            st.setFieldAsList("date", dates);
            st.setFieldAsList("bytes", bytes);
            st.setFieldAsList("isdir", isdirs);
            st.setFieldAsList("datenum", datenums);
            retval << st;
        }
    }
    return retval;
}
//=============================================================================
