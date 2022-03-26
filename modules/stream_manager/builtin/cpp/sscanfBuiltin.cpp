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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstdio>
#include <boost/filesystem.hpp>
#include "sscanfBuiltin.hpp"
#include "Error.hpp"
#include "File.hpp"
#include "FilesManager.hpp"
#include "Interface.hpp"
#include "characters_encoding.hpp"
#include "FscanFunction.hpp"
#include "FileSeek.hpp"
#include "Exception.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::sscanfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 4);
    bool isParam1SupportedType
        = (argIn[0].isStringArray() && argIn[0].isScalar() || argIn[0].isRowVectorCharacterArray());

    if (!isParam1SupportedType) {
        Error(_("First argument must be a text scalar."));
    }
    std::wstring wstr = argIn[0].getContentAsWideString();
    ArrayOf param2 = argIn[1];
    std::string format = param2.getContentAsCString();

    double m = -1, n = -1;
    bool haveThirdArgument = false;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        param3.promoteType(NLS_DOUBLE);
        Dimensions dims3 = param3.getDimensions();
        if (param3.isDoubleType(true)) {
            if (dims3.isScalar() || dims3.getElementCount() == 2) {
                if (dims3.isScalar()) {
                    m = param3.getContentAsDoubleScalar();
                    if (m < 0) {
                        Error(_W("Wrong value >= 0 expected."));
                    }
                } else {
                    double* ptr = (double*)param3.getDataPointer();
                    m = ptr[0];
                    if (m < 0) {
                        Error(_W("Wrong value >= 0 expected."));
                    }
                    n = ptr[1];
                    if (n < 0) {
                        Error(_W("Wrong value >= 0 expected."));
                    }
                }
            } else {
                Error(_W("Wrong size. scalar or [a, b] expected."));
            }
        } else {
            Error(_W("Wrong type. double expected."));
        }
        haveThirdArgument = true;
    }
    boost::filesystem::path tempFilePath = boost::filesystem::temp_directory_path();
    tempFilePath /= boost::filesystem::unique_path();
#ifdef _MSC_VER
    const std::wstring filenameTemp = tempFilePath.wstring();
#else
    const std::string filenameTemp = tempFilePath.string();
#endif
    std::wofstream wof(filenameTemp, std::ios::trunc | std::ios::binary);
    FILE* fp = nullptr;
    if (wof.is_open()) {
        wof << wstr;
        wof.close();
#ifdef _MSC_VER
        fp = _wfopen(filenameTemp.c_str(), L"rb");
#else
        fp = fopen(filenameTemp.c_str(), "rb");
#endif
    }
    if (fp == nullptr) {
        if (nLhs > 1) {
            retval << ArrayOf::doubleConstructor(0);
        }
        std::wstring errorMessage = _W("Cannot create temporary file.");
        if (nLhs > 2) {
            retval << ArrayOf::characterArrayConstructor(errorMessage);
        } else {
            Error(errorMessage);
        }
        if (nLhs > 3) {
            retval << ArrayOf::doubleConstructor(0);
        }
        return retval;
    }
    indexType count = 0;
    ArrayOf value;
    std::wstring errorMessage;
    try {
        value = FscanF(fp, format, "", m, n, haveThirdArgument, count, true);
        retval << value;
        bool isMalFormated = (feof(fp) == 0);
        if (isMalFormated) {
            errorMessage = _W("Matching failure in format.");
        }
    } catch (Exception& e) {
        errorMessage = e.getMessage();
        value = ArrayOf::emptyConstructor();
        retval << value;
    }
    int filePosition = ftell(fp) + 1;
    fclose(fp);
#ifdef _MSC_VER
    _wremove(filenameTemp.c_str());
#else
    remove(filenameTemp.c_str());
#endif
    if (nLhs > 1) {
        retval << ArrayOf::doubleConstructor((double)count);
    }
    if (nLhs > 2) {
        retval << ArrayOf::characterArrayConstructor(errorMessage);
    } else {
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
    }
    if (nLhs > 3) {
        retval << ArrayOf::doubleConstructor(filePosition);
    }

    return retval;
}
//=============================================================================
