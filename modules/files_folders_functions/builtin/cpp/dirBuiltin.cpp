//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "dirBuiltin.hpp"
#include "Error.hpp"
#include "GetCurrentDirectory.hpp"
#include "ListFiles.hpp"
#include "StringFormat.hpp"
#include <boost/container/vector.hpp>
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::dirBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    std::wstring wpath;
    std::wstring woption;
    bool bSubDirectories = false;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    switch (argIn.size()) {
    case 0: {
        wpath = GetCurrentDirectory();
        if (wpath == L"") {
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
        Interface* io = eval->getInterface();
        if (res.empty()) {
            std::wstring msg = std::wstring(L"\'") + wpath + std::wstring(L"\' ")
                + _W("Not a file or a directory.");
            io->outputMessage(msg);
        } else {
            for (boost::container::vector<FileInfo>::iterator it = res.begin(); it != res.end();
                 ++it) {
                if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                    break;
                }
                if (it->isDir()) {
                    if (it->getName() == L"." || it->getName() == L"..") {
                        io->outputMessage(it->getName() + L"\n");
                    } else {
                        io->outputMessage(it->getName() + L"/" + L"\n");
                    }
                } else {
                    io->outputMessage(it->getName() + L"\n");
                }
            }
        }
    } else {
        stringVector fieldnames;
        fieldnames.push_back("name");
        fieldnames.push_back("date");
        fieldnames.push_back("bytes");
        fieldnames.push_back("isdir");
        fieldnames.push_back("datenum");
        Dimensions dims;
        dims[0] = res.size();
        dims[1] = 1;
        if (res.empty()) {
            retval.push_back(ArrayOf::emptyStructConstructor(fieldnames, dims));
        } else {
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(
                NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames);
            ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
            ArrayOfVector names;
            ArrayOfVector dates;
            ArrayOfVector bytes;
            ArrayOfVector isdirs;
            ArrayOfVector datenums;
            names.reserve(res.size());
            dates.reserve(res.size());
            bytes.reserve(res.size());
            isdirs.reserve(res.size());
            datenums.reserve(res.size());
            for (boost::container::vector<FileInfo>::iterator it = res.begin(); it != res.end();
                 ++it) {
                names.push_back(ArrayOf::characterArrayConstructor(it->getName()));
                dates.push_back(ArrayOf::characterArrayConstructor(it->getDate()));
                double bytesval = it->getBytes();
                if (bytesval == -1) {
                    bytes.push_back(ArrayOf::emptyConstructor());
                } else {
                    bytes.push_back(ArrayOf::doubleConstructor(bytesval));
                }
                isdirs.push_back(ArrayOf::logicalConstructor(it->isDir()));
                double datenumval = it->getDatenum();
                if (datenumval == -1) {
                    datenums.push_back(ArrayOf::emptyConstructor());
                } else {
                    datenums.push_back(ArrayOf::doubleConstructor(datenumval));
                }
            }
            st.setFieldAsList("name", names);
            st.setFieldAsList("date", dates);
            st.setFieldAsList("bytes", bytes);
            st.setFieldAsList("isdir", isdirs);
            st.setFieldAsList("datenum", datenums);
            retval.push_back(st);
        }
    }
    return retval;
}
//=============================================================================
