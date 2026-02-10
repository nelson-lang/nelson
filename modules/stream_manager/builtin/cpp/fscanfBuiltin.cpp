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
#include "i18n.hpp"
#include "File.hpp"
#include "FilesManager.hpp"
#include "Interface.hpp"
#include "characters_encoding.hpp"
#include "FscanFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
    double dID = param1.getContentAsDoubleScalar();
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    auto iValue = static_cast<int32>(dID);
    if (fm == nullptr) {
        raiseError(L"Nelson:stream_manager:ERROR_PROBLEM_WITH_FILE_MANAGER",
            ERROR_PROBLEM_WITH_FILE_MANAGER);
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
                        raiseError(L"Nelson:stream_manager:ERROR_WRONG_VALUE_GE_0_EXPECTED",
                            ERROR_WRONG_VALUE_GE_0_EXPECTED);
                    }
                    n = 1;
                } else {
                    double* ptr = (double*)param3.getDataPointer();
                    m = ptr[0];
                    if (m < 0) {
                        raiseError(L"Nelson:stream_manager:ERROR_WRONG_VALUE_GE_0_EXPECTED",
                            ERROR_WRONG_VALUE_GE_0_EXPECTED);
                    }
                    n = ptr[1];
                    if (n < 0) {
                        raiseError(L"Nelson:stream_manager:ERROR_WRONG_VALUE_GE_0_EXPECTED",
                            ERROR_WRONG_VALUE_GE_0_EXPECTED);
                    }
                }
            } else {
                raiseError(L"Nelson:stream_manager:ERROR_WRONG_SIZE_SCALAR_OR_A_B_EXPECTED",
                    ERROR_WRONG_SIZE_SCALAR_OR_A_B_EXPECTED);
            }
        } else {
            raiseError(L"Nelson:stream_manager:ERROR_WRONG_TYPE_DOUBLE_EXPECTED",
                ERROR_WRONG_TYPE_DOUBLE_EXPECTED);
        }
        haveThirdArgument = true;
    }
    if (!fm->isOpened(iValue)) {
        raiseError(L"Nelson:stream_manager:ERROR_INVALID_FILE_ID_EXPECTED",
            ERROR_INVALID_FILE_ID_EXPECTED);
    }
    File* f = fm->getFile(iValue);
    if (f->isInterfaceMethod()) {
        raiseError(L"Nelson:stream_manager:ERROR_NOT_IMPLEMENTED_FOR_STDIO",
            ERROR_NOT_IMPLEMENTED_FOR_STDIO);
    }
    std::string encoding = wstring_to_utf8(f->getEncoding());
    FILE* filepointer = static_cast<FILE*>(f->getFilePointer());
    if (!filepointer) {
        raiseError(L"Nelson:stream_manager:ERROR_INVALID_FILE_ID_EXPECTED",
            ERROR_INVALID_FILE_ID_EXPECTED);
    }
    indexType count = 0;
    ArrayOf value = FscanF(filepointer, format, encoding, m, n, haveThirdArgument, count, false);
    retval << value;
    if (nLhs > 1) {
        retval << ArrayOf::doubleConstructor((double)count);
    }
    return retval;
}
//=============================================================================
