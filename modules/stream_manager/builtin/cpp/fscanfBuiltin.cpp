//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "fscanfBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FilesManager.hpp"
#include "Interface.hpp"
#include "characters_encoding.hpp"
#include "FscanFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fscanfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    ArrayOf param1 = argIn[0];
    double dID = param1.asDoubleScalar();
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    auto iValue = static_cast<int32>(dID);
    if (fm == nullptr) {
        Error(L"Nelson:stream_manager:fileManagerNotAvailable", L"Problem with file manager.");
    }
    ArrayOf param2 = argIn[1];
    std::string format = param2.asUtf8String();

    double m = -1, n = -1;
    bool haveThirdArgument = false;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        param3.promoteType(NLS_DOUBLE);
        Dimensions dims3 = param3.dimensions();
        if (param3.isDoubleType(true)) {
            if (dims3.isScalar() || dims3.elementCount() == 2) {
                if (dims3.isScalar()) {
                    m = param3.asDoubleScalar();
                    if (m < 0) {
                        Error(L"Nelson:stream_manager:invalidSizeValue",
                            L"Wrong value >= 0 expected.");
                    }
                    n = 1;
                } else {
                    double* ptr = (double*)param3.dataPointer();
                    m = ptr[0];
                    if (m < 0) {
                        Error(L"Nelson:stream_manager:invalidSizeValue",
                            L"Wrong value >= 0 expected.");
                    }
                    n = ptr[1];
                    if (n < 0) {
                        Error(L"Nelson:stream_manager:invalidSizeValue",
                            L"Wrong value >= 0 expected.");
                    }
                }
            } else {
                Error(L"Nelson:stream_manager:invalidSize",
                    L"Wrong size. scalar or [a, b] expected.");
            }
        } else {
            Error(L"Nelson:stream_manager:invalidSizeType", L"Wrong type. double expected.");
        }
        haveThirdArgument = true;
    }
    if (!fm->isOpened(iValue)) {
        Error(L"Nelson:stream_manager:invalidFileIdentifierArgument",
            L"Wrong value for #%d argument: a valid file ID expected.", 1);
    }
    File* f = fm->getFile(iValue);
    if (f->isInterfaceMethod()) {
        Error(L"Nelson:stream_manager:notImplementedForStandardStream",
            L"Not implemented for 'stdout', 'stderr' or 'stdin'.");
    }
    std::string encoding = wstring_to_utf8(f->getEncoding());
    FILE* filepointer = static_cast<FILE*>(f->getFilePointer());
    if (!filepointer) {
        Error(L"Nelson:stream_manager:invalidFileIdentifierArgument",
            L"Wrong value for #%d argument: a valid file ID expected.", 1);
    }
    indexType count = 0;
    ArrayOf value = FscanF(filepointer, format, encoding, m, n, haveThirdArgument, count, false);
    retval << value;
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar((double)count);
    }
    return retval;
}
//=============================================================================
