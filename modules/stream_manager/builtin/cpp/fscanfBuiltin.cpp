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
#include "fscanfBuiltin.hpp"
#include "Error.hpp"
#include "File.hpp"
#include "FilesManager.hpp"
#include "Interface.hpp"
#include "characters_encoding.hpp"
#include "FscanFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fscanfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    ArrayOf param1 = argIn[0];
    double dID = param1.getContentAsDoubleScalar();
    auto* fm = static_cast<FilesManager*>(eval->FileManager);
    auto iValue = static_cast<int32>(dID);
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
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
    if (!fm->isOpened(iValue)) { //-V1004
        Error(_W("Wrong value for #1 argument: a valid file ID expected."));
    }
    File* f = fm->getFile(iValue);
    if (f->isInterfaceMethod()) {
        Error(_W("Not implemented for 'stdout', 'stderr' or 'stdin'."));
    }
    std::string encoding = wstring_to_utf8(f->getEncoding());
    FILE* filepointer = static_cast<FILE*>(f->getFilePointer());
    if (!filepointer) {
        Error(_W("Wrong value for #1 argument: a valid file ID expected."));
    }
    indexType count = 0;
    ArrayOf value = FscanF(filepointer, format, encoding, m, n, haveThirdArgument, count);
    retval << value;
    if (nLhs > 1) {
        retval << ArrayOf::doubleConstructor((double)count);
    }
    return retval;
}
//=============================================================================
