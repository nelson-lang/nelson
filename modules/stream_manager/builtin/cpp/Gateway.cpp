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
#include "FilesManager.hpp"
#include "Interface.hpp"
#include "NelsonGateway.hpp"
#include "diaryBuiltin.hpp"
#include "dlmwriteBuiltin.hpp"
#include "fcloseBuiltin.hpp"
#include "fgetlBuiltin.hpp"
#include "fgetsBuiltin.hpp"
#include "filereadBuiltin.hpp"
#include "filewriteBuiltin.hpp"
#include "fopenBuiltin.hpp"
#include "fprintfBuiltin.hpp"
#include "freadBuiltin.hpp"
#include "frewindBuiltin.hpp"
#include "fseekBuiltin.hpp"
#include "fsizeBuiltin.hpp"
#include "ftellBuiltin.hpp"
#include "fwriteBuiltin.hpp"
#include "loadBuiltin.hpp"
#include "saveBuiltin.hpp"
#include "feofBuiltin.hpp"
#include "ferrorBuiltin.hpp"
#include "fscanfBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"stream_manager";
//=============================================================================
static const nlsGateway gateway[] = { { "diary", (void*)Nelson::StreamGateway::diaryBuiltin, 1, 1,
                                          CPP_BUILTIN_WITH_EVALUATOR },
    { "fopen", (void*)Nelson::StreamGateway::fopenBuiltin, 4, 4, CPP_BUILTIN_WITH_EVALUATOR },
    { "fclose", (void*)Nelson::StreamGateway::fcloseBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "fwrite", (void*)Nelson::StreamGateway::fwriteBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "fread", (void*)Nelson::StreamGateway::freadBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "fprintf", (void*)Nelson::StreamGateway::fprintfBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "fgetl", (void*)Nelson::StreamGateway::fgetlBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "fgets", (void*)Nelson::StreamGateway::fgetsBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "ftell", (void*)Nelson::StreamGateway::ftellBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "frewind", (void*)Nelson::StreamGateway::frewindBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "fseek", (void*)Nelson::StreamGateway::fseekBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "fsize", (void*)Nelson::StreamGateway::fsizeBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "dlmwrite", (void*)Nelson::StreamGateway::dlmwriteBuiltin, 0, -3, CPP_BUILTIN_WITH_EVALUATOR },
    { "fileread", (void*)Nelson::StreamGateway::filereadBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "filewrite", (void*)Nelson::StreamGateway::filewriteBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "load", (void*)Nelson::StreamGateway::loadBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "save", (void*)Nelson::StreamGateway::saveBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "feof", (void*)Nelson::StreamGateway::feofBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "ferror", (void*)Nelson::StreamGateway::ferrorBuiltin, 2, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "fscanf", (void*)Nelson::StreamGateway::fscanfBuiltin, 2, 2, CPP_BUILTIN_WITH_EVALUATOR } };
//=============================================================================
static bool
initializeModule(Nelson::Evaluator* eval)
{
    if (eval->FileManager == nullptr) {
        Interface* io = eval->getInterface();
        Nelson::FilesManager* fm;
        try {
            fm = new Nelson::FilesManager(io);
        } catch (const std::bad_alloc&) {
            fm = nullptr;
        }
        if (fm) {
            eval->FileManager = (void*)fm;
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
finishModule(Nelson::Evaluator* eval)
{
    if (eval->FileManager != nullptr) {
        Nelson::FilesManager* fm = (Nelson::FilesManager*)eval->FileManager;
        delete fm;
        eval->FileManager = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
