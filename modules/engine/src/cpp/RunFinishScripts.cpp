//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "FileSystemWrapper.hpp"
#include "RunFinishScripts.hpp"
#include "CloseAllFiles.hpp"
#include "EvaluateScriptFile.hpp"
#include "Interface.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "PathFunctionIndexerManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RunFinishScripts(Evaluator* eval)
{
    if ((eval == nullptr) || (eval->getContext() == nullptr)) {
        return false;
    }
    wstringVector listPath = PathFunctionIndexerManager::getInstance()->getPathNameVector(true);
    for (auto& elementPath : listPath) {
        FileSystemWrapper::Path path(elementPath);
        path += L"/finish.m";
        bool bIsFile = FileSystemWrapper::Path::is_regular_file(path);
        if (bIsFile) {
            std::wstring wstr = path.generic_wstring();
            try {
                EvaluateScriptFile(eval, wstr);
            } catch (const Exception& e) {
                eval->setState(NLS_STATE_CANCEL_QUIT);
                CloseAllFiles();
                Interface* io = eval->getInterface();
                eval->setLastErrorException(e);
                std::wstring errmsg = fmt::sprintf(_W("'%s' failed to run."), wstr);
                if (io != nullptr) {
                    io->errorMessage(errmsg);
                    return false;
                } else {
                    errmsg = errmsg + L"\n";
                    fwprintf(stderr, L"%ls", errmsg.c_str());
                }
            }
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
