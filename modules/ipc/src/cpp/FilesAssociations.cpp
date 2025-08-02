//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include <string>
#include "FileSystemWrapper.hpp"
#include "FilesAssociation.hpp"
#include "EvaluateCommand.hpp"
#include "NelSon_engine_mode.h"
#include "PostCommandDynamicFunction.hpp"
#include "NelsonPIDs.hpp"
#include "NelsonInterprocess.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
commonFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const std::wstring& command, const wstringVector& filesToOpen);
//=============================================================================
bool
OpenFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen, bool sendByIPC)
{
    if (sendByIPC) {
        int existingPID = getLatestPidWithModeInSharedMemory(currentMode);
        if (existingPID == 0) {
            return false;
        }
        return sendCommandFileExtensionToNelsonInterprocessReceiver(
            existingPID, NELSON_INTERPROCESS_COMMAND::OPEN_FILES, filesToOpen);
    }
    return commonFilesAssociated(currentMode, L"edit", filesToOpen);
}
//=============================================================================
bool
LoadFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen, bool sendByIPC)
{
    if (sendByIPC) {
        int existingPID = getLatestPidWithModeInSharedMemory(currentMode);
        if (existingPID == 0) {
            return false;
        }
        return sendCommandFileExtensionToNelsonInterprocessReceiver(
            existingPID, NELSON_INTERPROCESS_COMMAND::OPEN_FILES, filesToOpen);
    }
    return commonFilesAssociated(currentMode, L"load", filesToOpen);
}
//=============================================================================
bool
ExecuteFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen, bool sendByIPC)
{
    if (sendByIPC) {
        int existingPID = getLatestPidWithModeInSharedMemory(currentMode);
        if (existingPID == 0) {
            return false;
        }
        return sendCommandFileExtensionToNelsonInterprocessReceiver(
            existingPID, NELSON_INTERPROCESS_COMMAND::RUN_FILES, filesToOpen);
    }
    return commonFilesAssociated(currentMode, L"run", filesToOpen);
}
//=============================================================================
bool
commonFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const std::wstring& command, const wstringVector& filesToOpen)
{
    bool res = false;
    if (currentMode == NELSON_ENGINE_MODE::GUI) {
        if (!filesToOpen.empty()) {
            try {
                for (const auto& k : filesToOpen) {
                    FileSystemWrapper::Path pathFileToOpen(k);
                    bool bIsFile = FileSystemWrapper::Path::is_regular_file(pathFileToOpen);
                    if (bIsFile) {
                        std::wstring commandToExecute = command + std::wstring(L"('" + k + L"');");
                        bool r = PostCommandDynamicFunction(commandToExecute);
                        if (r != true) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                return true;
            } catch (Exception&) {
                res = false;
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
