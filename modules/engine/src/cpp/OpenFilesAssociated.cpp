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
#include "OpenFilesAssociated.hpp"
#include "EvaluateCommand.hpp"
#include "NelSon_engine_mode.h"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
OpenFilesAssociated(Evaluator* eval, wstringVector filesToOpen)
{
    bool res = false;
    if (eval->getNelsonEngineMode() == NELSON_ENGINE_MODE::ADVANCED_TERMINAL
        || eval->getNelsonEngineMode() == NELSON_ENGINE_MODE::GUI) {
        if (filesToOpen.size() > 0) {
            try {
                for (size_t k = 0; k < filesToOpen.size(); k++) {
                    boost::filesystem::path pathFileToOpen(filesToOpen[k]);
                    bool bIsFile = boost::filesystem::exists(pathFileToOpen)
                        && !boost::filesystem::is_directory(pathFileToOpen);
                    if (bIsFile) {
                        std::wstring editCommand = std::wstring(L"edit('" + filesToOpen[k] + L"')");
                        EvaluateCommand(eval, editCommand.c_str(), false);
                        res = true;
                    }
                }
            } catch (Exception& e) {
                Interface* io = eval->getInterface();
                io->errorMessage(e.getMessage());
                res = false;
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
