//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "StartNelsonMainScript.hpp"
#include "CloseAllFiles.hpp"
#include "EvaluateScriptFile.hpp"
#include "Interface.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
bool
StartNelsonMainScript(Evaluator* eval)
{
    Context* ctx = eval->getContext();
    if (ctx != nullptr) {
        std::wstring rootPath = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
        FileSystemWrapper::Path path(rootPath);
        path += L"/etc/startup.m";
        bool bIsFile = FileSystemWrapper::Path::is_regular_file(path);
        if (bIsFile) {
            bool succeeded = true;
            NelsonConfiguration::getInstance()->disableModulesProtection();
            std::wstring wstr = path.generic_wstring();
            try {
                EvaluateScriptFile(eval, wstr);
            } catch (NelsonException& e) {
                succeeded = false;
                CloseAllFiles();
                Interface* io = eval->getInterface();
                eval->setLastErrorException(e);
                std::wstring errmsg = resolveMessageTemplate(
                    L"Nelson:engine:mainStartupMFailed", L"Main startup.m failed to run.");
                if (io != nullptr) {
                    io->errorMessage(errmsg);
                    io->errorMessage(e.formattedErrorMessage() + L"\n");
                } else {
                    errmsg = errmsg + L"\n";
                    fwprintf(stderr, L"%ls", errmsg.c_str());
                    fwprintf(stderr, L"%ls", e.formattedErrorMessage().c_str());
                    fwprintf(stderr, L"%ls", L"\n");
                }
            }
            NelsonConfiguration::getInstance()->enableModulesProtection();
            return succeeded;
        }
        return false;
    }
    return false;
}
//=============================================================================
