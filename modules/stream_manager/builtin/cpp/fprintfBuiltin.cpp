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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FilesManager.hpp"
#include "Interface.hpp"
#include "PrintfFunction.hpp"
#include "AnsiSgr.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadHelpers.hpp"
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
        if (argIn.size() < 2) {
            Error(L"Nelson:stream_manager:noFormat", L"No format character vector.");
        }
        dID = param1.asDoubleScalar();
        ArrayOf param2 = argIn[1];
        bool isSupported
            = param2.isCharacterArray() || (param2.isStringArray() && param2.isScalar());
        if (!isSupported) {
            Error(L"Nelson:stream_manager:invalidFormat", L"Invalid format.");
        }
        firstArgumentPosition = 1;
    } else if (param1.isRowVectorCharacterArray()
        || (param1.isStringArray() && param1.isScalar())) {
        dID = 1;
        firstArgumentPosition = 0;
    } else {
        Error(L"Nelson:stream_manager:invalidFormat", L"Invalid format.");
    }
    for (size_t i = firstArgumentPosition; i < argIn.size(); i++) {
        args.push_back(argIn[i]);
    }
    args = convertObjectPrintfArguments(eval, args);
    if (!printfFunction(args, errorMessage, result)) {
        Error(L"Nelson:fprintf:formatFailed", errorMessage);
    }
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    auto iValue = static_cast<int32>(dID);
    if (fm == nullptr) {
        Error(L"Nelson:stream_manager:fileManagerNotAvailable", L"Problem with file manager.");
    }
    size_t len = 0;
    if (fm->isOpened(iValue)) {
        File* f = fm->getFile(iValue);
        if (f->isInterfaceMethod()) {
            if ((f->getFileName() == L"stdout") || (f->getFileName() == L"stderr")) {
                Interface* io = eval->getInterface();
                if (io) {
                    io->outputAnsiText(result, f->getFileName() == L"stderr");
                }
                len = ansiVisibleLength(result);
            } else {
                Error(L"Nelson:fprintf:idNotSupported", L"ID not supported.");
            }
        } else {
            FILE* filepointer = static_cast<FILE*>(f->getFilePointer());
            if (filepointer) {
                std::wstring encoding = f->getEncoding();
                std::string data = wstring_to_utf8(result);
                if (encoding == L"UTF-8") {
                    size_t written = fwrite(data.c_str(), 1, data.size(), filepointer);
                    len = (size_t)written;
                } else {
                    std::string converted = data;
                    if (utf8ToCharsetConverter(converted, converted, wstring_to_utf8(encoding))) {
                        size_t written
                            = fwrite(converted.c_str(), 1, converted.size(), filepointer);
                        len = (size_t)written;
                    } else {
                        Error(L"Nelson:stream_manager:invalidEncoding", L"Cannot use encoding: %s",
                            encoding);
                    }
                }

            } else {
                Error(L"Nelson:fprintf:idNotSupported", L"ID not supported.");
            }
        }
        if (nLhs > 0) {
            retval << ArrayOf::doubleScalar((double)len);
        }
    } else {
        Error(L"Nelson:stream_manager:invalidFileIdentifierArgument",
            L"Wrong value for #%d argument: a valid file ID expected.", 1);
    }
    return retval;
}
//=============================================================================
