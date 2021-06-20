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
#include "StartNelsonUserModules.hpp"
#include "CloseAllFiles.hpp"
#include "EvaluateScriptFile.hpp"
#include "Interface.hpp"
#include "ReadUserModules.hpp"
#include "ModulesManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
printError(Interface* io, const std::wstring& message)
{
    if (io != nullptr) {
        io->warningMessage(message);
    } else {
        const wchar_t* format = L"%s\n";
        fwprintf(stdout, format, message.c_str()); // lgtm [cpp/wrong-type-format-argument]
    }
}
//=============================================================================
bool
StartNelsonUserModules(Evaluator* eval)
{
    std::vector<std::tuple<std::wstring, std::wstring, bool>> userModules;
    bool bOK = ReadUserModules(userModules);
    if (bOK) {
        Context* ctx = eval->getContext();
        if (ctx != nullptr) {
            for (auto element : userModules) {
                std::wstring name = std::get<0>(element);
                if (IsExistingModuleName(name)) {
                    UnregisterModule(name);
                    Interface* io = eval->getInterface();
                    std::wstring msg = _W("Module already loaded: ") + name;
                    printError(io, msg);
                } else {
                    std::wstring path = std::get<1>(element);
                    bool load = std::get<2>(element);
                    if (load) {
                        std::wstring loader = path + std::wstring(L"/loader.nls");
                        try {
                            EvaluateScriptFile(eval, loader, true);
                        } catch (const Exception& e) {
                            if (IsExistingModuleName(name)) {
                                UnregisterModule(name);
                            }
                            CloseAllFiles();
                            Interface* io = eval->getInterface();
                            eval->setLastErrorException(e);
                            std::wstring errmsg = _W("Impossible to load module: ") + name;
                            printError(io, errmsg);
                        }
                    }
                }
            }
        }
    } else {
        if (!userModules.empty()) {
            Interface* io = eval->getInterface();
            std::wstring msg = _W("Impossible to read modules.json");
            printError(io, msg);
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
