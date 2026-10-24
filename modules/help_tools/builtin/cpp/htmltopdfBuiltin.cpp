//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include "htmltopdfBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HtmlToPdf.hpp"
#include "FileSystemWrapper.hpp"
#include "NelSon_engine_mode.h"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::htmltopdfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf arg1 = argIn[0];
    ArrayOf arg2 = argIn[1];
    if (!arg1.isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (!arg2.isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    std::wstring param1 = arg1.asWideString();
    std::wstring param2 = arg2.asWideString();
    if (!FileSystemWrapper::Path::is_regular_file(param1)) {
        Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (1));
    }
    auto _mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    switch (_mode) {
    case ADVANCED_ENGINE:
    case ADVANCED_TERMINAL:
    case GUI: {
        FileSystemWrapper::Path pdfname(param2);
        if (pdfname.extension().string() != ".pdf") {
            pdfname.replace_extension(FileSystemWrapper::Path(".pdf"));
        }
        std::ofstream pdffile;
#if _MSC_VER
        pdffile.open(pdfname.generic_wstring());
#else
        pdffile.open(pdfname.generic_string());
#endif
        if (!pdffile.is_open()) {
            Error(L"Nelson:htmltopdf:cannotOpenDestinationFile",
                L"Cannot not open destination file.");
        }
        pdffile.close();
        if (!HtmlFileToPdfFile(param1, param2)) {
            Error(L"Nelson:htmltopdf:pdfNotGenerated", L"pdf file not generated.");
        }
    } break;
    default: {
        Error(L"Nelson:htmltopdf:pdfNotSupportedInEngineMode",
            L"pdf cannot generated in this engine mode.");
    } break;
    }
    return retval;
}
//=============================================================================
