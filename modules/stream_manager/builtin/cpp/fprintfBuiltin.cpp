//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fprintfBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FilesManager.hpp"
#include "Interface.hpp"
#include "PrintfFunction.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fprintfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    double dID = 1;
    std::wstring result;
    std::wstring errorMessage;
    indexType firstArgumentPosition = 0;
    ArrayOfVector args;
    if (param1.isDoubleType() && param1.isScalar()) {
        dID = param1.getContentAsDoubleScalar();
        ArrayOf param2 = argIn[1];
        bool isSupported
            = param2.isCharacterArray() || (param2.isStringArray() && param2.isScalar());
        if (!isSupported) {
            raiseError2(L"nelson:arguments:wrongNumberOfOutputs");
        }
        firstArgumentPosition = 1;
    } else if (param1.isRowVectorCharacterArray()
        || (param1.isStringArray() && param1.isScalar())) {
        dID = 1;
        firstArgumentPosition = 0;
    } else {
        raiseError(
            L"Nelson:stream_manager:ERROR_VALID_FORMAT_EXPECTED", ERROR_VALID_FORMAT_EXPECTED);
    }
    for (size_t i = firstArgumentPosition; i < argIn.size(); i++) {
        args.push_back(argIn[i]);
    }
    if (!printfFunction(args, errorMessage, result)) {
        Error(errorMessage, L"Nelson:stream_manager:ERROR_PRINTF_ERROR");
    }
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    auto iValue = static_cast<int32>(dID);
    if (fm == nullptr) {
        raiseError(L"Nelson:stream_manager:ERROR_PROBLEM_WITH_FILE_MANAGER",
            ERROR_PROBLEM_WITH_FILE_MANAGER);
    }
    size_t len = 0;
    if (fm->isOpened(iValue)) {
        File* f = fm->getFile(iValue);
        if (f->isInterfaceMethod()) {
            if ((f->getFileName() == L"stdout") || (f->getFileName() == L"stderr")) {
                Interface* io = eval->getInterface();
                if (io) {
                    if (f->getFileName() == L"stdout") {
                        io->outputMessage(result);
                    } else {
                        io->errorMessage(result);
                    }
                }
                len = result.length();
            } else {
                raiseError(L"Nelson:stream_manager:ERROR_ID_NOT_SUPPORTED", ERROR_ID_NOT_SUPPORTED);
            }
        } else {
            FILE* filepointer = static_cast<FILE*>(f->getFilePointer());
            if (filepointer) {
                std::wstring encoding = f->getEncoding();
                if (encoding == L"UTF-8") {
                    fwprintf(filepointer, L"%ls", result.c_str());
                    len = result.length();
                } else {
                    std::string data = wstring_to_utf8(result);
                    if (utf8ToCharsetConverter(data, data, wstring_to_utf8(encoding))) {
                        fprintf(filepointer, "%s", data.c_str());
                        len = data.length();
                    } else {
                        raiseError(L"Nelson:stream_manager:ERROR_CANNOT_USE_ENCODING",
                            ERROR_CANNOT_USE_ENCODING, encoding);
                    }
                }

            } else {
                raiseError(L"Nelson:stream_manager:ERROR_ID_NOT_SUPPORTED", ERROR_ID_NOT_SUPPORTED);
            }
        }
        if (nLhs > 0) {
            retval << ArrayOf::doubleConstructor((double)len);
        }
    } else {
        raiseError(L"Nelson:stream_manager:ERROR_INVALID_FILE_ID_EXPECTED",
            ERROR_INVALID_FILE_ID_EXPECTED);
    }
    return retval;
}
//=============================================================================
