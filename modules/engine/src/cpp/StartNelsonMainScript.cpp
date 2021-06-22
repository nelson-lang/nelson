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
#include <boost/filesystem.hpp>
#include "StartNelsonMainScript.hpp"
#include "CloseAllFiles.hpp"
#include "EvaluateScriptFile.hpp"
#include "GetNelsonPath.hpp"
#include "Interface.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
bool
StartNelsonMainScript(Evaluator* eval)
{
    Context* ctx = eval->getContext();
    if (ctx != nullptr) {
        std::wstring rootPath = Nelson::GetRootPath();
        boost::filesystem::path path(rootPath);
        path += L"/etc/startup.m";
        bool bIsFile = boost::filesystem::exists(path) && !boost::filesystem::is_directory(path);
        if (bIsFile) {
            NelsonConfiguration::getInstance()->disableModulesProtection();
            std::wstring wstr = path.generic_wstring();
            try {
                EvaluateScriptFile(eval, wstr);
            } catch (Exception& e) {
                // close all files in case of error at start-up
                CloseAllFiles();
                Interface* io = eval->getInterface();
                eval->setLastErrorException(e);
                std::wstring errmsg = _W("Main startup.m failed to run.");
                if (io != nullptr) {
                    io->errorMessage(errmsg);
                    io->errorMessage(e.getFormattedErrorMessage() + L"\n");
                } else {
                    errmsg = errmsg + L"\n";
                    fwprintf(stderr, L"%ls", errmsg.c_str());
                    fwprintf(stderr, L"%ls", e.getFormattedErrorMessage().c_str());
                    fwprintf(stderr, L"%ls", L"\n");
                }
            }
            NelsonConfiguration::getInstance()->enableModulesProtection();
            return true;
        }
        return false;
    }
    return false;
}
//=============================================================================
