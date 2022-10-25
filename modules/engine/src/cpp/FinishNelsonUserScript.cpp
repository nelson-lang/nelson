//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "FinishNelsonUserScript.hpp"
#include "CloseAllFiles.hpp"
#include "EvaluateScriptFile.hpp"
#include "GetPreferencesPath.hpp"
#include "Interface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
FinishNelsonUserScript(Evaluator* eval)
{
    Context* ctx = eval->getContext();
    if (ctx != nullptr) {
        std::wstring prefPath = GetPreferencesPath();
        FileSystemWrapper::Path path(prefPath);
        path += L"/finish.m";
        bool bIsFile = FileSystemWrapper::Path::is_regular_file(path);
        if (bIsFile) {
            std::wstring wstr = path.generic_wstring();
            try {
                EvaluateScriptFile(eval, wstr);
            } catch (const Exception& e) {
                CloseAllFiles();
                Interface* io = eval->getInterface();
                eval->setLastErrorException(e);
                std::wstring errmsg = _W("User finish.m failed to run.");
                if (io != nullptr) {
                    io->errorMessage(errmsg);
                } else {
                    errmsg = errmsg + L"\n";
                    fwprintf(stderr, L"%ls", errmsg.c_str());
                }
            }
            return true;
        }
        return false;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
