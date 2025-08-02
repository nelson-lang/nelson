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
#include "i18n.hpp"
#include "HtmlToPdf.hpp"
#include "FileSystemWrapper.hpp"
#include "NelSon_engine_mode.h"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
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
    if (arg1.isRowVectorCharacterArray() && arg2.isRowVectorCharacterArray()) {
        std::wstring param1 = arg1.getContentAsWideString();
        std::wstring param2 = arg2.getContentAsWideString();
        if (!FileSystemWrapper::Path::is_regular_file(param1)) {
            Error(ERROR_WRONG_ARGUMENT_1_VALUE);
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
                Error(_W("Cannot not open destination file."));
            }
            pdffile.close();
            if (!HtmlFileToPdfFile(param1, param2)) {
                Error(_W("pdf file not generated."));
            }
        } break;
        default: {
            Error(_W("pdf cannot generated in this engine mode."));
        } break;
        }
    } else {
        Error(ERROR_WRONG_ARGUMENTS_TYPE);
    }
    return retval;
}
//=============================================================================
