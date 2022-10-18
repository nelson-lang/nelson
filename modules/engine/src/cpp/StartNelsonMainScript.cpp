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
        Nelson::FileSystemWrapper::Path path(rootPath);
        path += L"/etc/startup.m";
        bool bIsFile = isFile(path);
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
