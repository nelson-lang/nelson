//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "dirBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GetCurrentDirectory.hpp"
#include "ListFiles.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 0: {
        wpath = GetCurrentDirectory();
        if (wpath.empty()) {
            Error(L"Nelson:files_folders_functions:impossibleGetCurrentDirectory",
                L"Impossible to get current directory.");
        }
    } break;
    case 2: {
        if (!argIn[1].isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
        woption = argIn[1].asWideString();
        if (woption == L"-s") {
            bSubDirectories = true;
        } else {
            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
        }
    }
    case 1: {
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        wpath = argIn[0].asWideString();
    } break;
    default:
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
        break;
    }
    std::vector<FileInfo> res = ListFiles(wpath, bSubDirectories);
    if (nLhs == 0) {
        if (res.empty()) {
            std::wstring msg = std::wstring(L"\'") + wpath + std::wstring(L"\' ")
                + _W("Not a file or a directory.");
            NelsonPrint(msg);
        } else {
            NelsonPrint("\n");
            for (auto& re : res) {
                if (NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())) {
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
            retval << ArrayOf::emptyStruct(fieldnames, dims);
        } else {
            auto* elements = static_cast<ArrayOf*>(
                ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldnames, false));
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
                names.push_back(ArrayOf::characterArray(re.getName()));
                folder.push_back(ArrayOf::characterArray(re.getFolder()));
                dates.push_back(ArrayOf::characterArray(re.getDate()));
                double bytesval = re.getBytes();
                if (bytesval == -1) {
                    bytes.push_back(ArrayOf::empty());
                } else {
                    bytes.push_back(ArrayOf::doubleScalar(bytesval));
                }
                isdirs.push_back(ArrayOf::logicalScalar(re.isDir()));
                double datenumval = re.getDatenum();
                if (datenumval == -1) {
                    datenums.push_back(ArrayOf::empty());
                } else {
                    datenums.push_back(ArrayOf::doubleScalar(datenumval));
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
