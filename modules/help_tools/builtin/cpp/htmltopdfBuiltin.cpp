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
#include "htmltopdfBuiltin.hpp"
#include "Error.hpp"
#include "HtmlToPdf.hpp"
#include "IsFile.hpp"
#include "NelSon_engine_mode.h"
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::htmltopdfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf arg1 = argIn[0];
    ArrayOf arg2 = argIn[1];
    if (arg1.isRowVectorCharacterArray() && arg2.isRowVectorCharacterArray()) {
        std::wstring param1 = arg1.getContentAsWideString();
        std::wstring param2 = arg2.getContentAsWideString();
        if (!IsFile(param1)) {
            Error(ERROR_WRONG_ARGUMENT_1_VALUE);
        }
        NELSON_ENGINE_MODE _mode = (NELSON_ENGINE_MODE)eval->getNelsonEngineMode();
        switch (_mode) {
        case ADVANCED_ENGINE:
        case ADVANCED_TERMINAL:
        case GUI: {
            boost::filesystem::path pdfname(param2);
            if (pdfname.extension().string() != ".pdf") {
                pdfname.replace_extension(".pdf");
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
