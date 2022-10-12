//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemHelpers.hpp"
#include "FinishNelsonMainScript.hpp"
#include "CloseAllFiles.hpp"
#include "EvaluateScriptFile.hpp"
#include "GetNelsonPath.hpp"
#include "Interface.hpp"
#include "NelsonConfiguration.hpp"
#include "GatewaysManager.hpp"
//=============================================================================
bool
FinishNelsonMainScript(Evaluator* eval)
{
    Context* ctx = eval->getContext();
    if (ctx != nullptr) {
        std::wstring rootPath = Nelson::GetRootPath();
        std::filesystem::path path = createFileSystemPath(rootPath);
        path += L"/etc/finish.m";
        if (isFile(path)) {
            NelsonConfiguration::getInstance()->disableModulesProtection();
            std::wstring wstr = convertFileSytemPathToGenericWString(path);
            try {
                EvaluateScriptFile(eval, wstr);
            } catch (const Exception& e) {
                CloseAllFiles();
                Interface* io = eval->getInterface();
                eval->setLastErrorException(e);
                std::wstring errmsg = _W("Main finish.m failed to run.");
                if (io != nullptr) {
                    io->errorMessage(errmsg);
                } else {
                    errmsg = errmsg + L"\n";
                    fwprintf(stderr, L"%ls", errmsg.c_str());
                }
            }
            GatewaysManager::getInstance()->destroy(eval);
            return true;
        }
        return false;
    }
    return false;
}
//=============================================================================
