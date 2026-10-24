//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ExamplesBridge.hpp"
#include <algorithm>
#include <filesystem>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
BuildExamplesCommand(const std::string& method, const std::wstring& path,
    const std::vector<std::wstring>& allowedPaths, std::wstring& command, std::string& error)
{
    command.clear();
    error.clear();
    if (method != "open" && method != "run") {
        error = "unsupported action";
        return false;
    }
    if (path.find_first_of(L"\r\n") != std::wstring::npos) {
        error = "example file path contains unsupported control characters";
        return false;
    }
    std::error_code filesystemError;
    const std::filesystem::path canonical
        = std::filesystem::weakly_canonical(path, filesystemError);
    if (filesystemError || !std::filesystem::is_regular_file(canonical)) {
        error = "example file is unavailable";
        return false;
    }
    const auto found = std::find(allowedPaths.begin(), allowedPaths.end(), canonical.wstring());
    if (found == allowedPaths.end()) {
        error = "example file is not in the current catalog";
        return false;
    }
    std::wstring escaped = canonical.wstring();
    size_t position = 0;
    while ((position = escaped.find(L'\'', position)) != std::wstring::npos) {
        escaped.insert(position, 1, L'\'');
        position += 2;
    }
    // An NFlow model has no script to run or edit: both actions open it in NFlow.
    if (canonical.extension() == L".nflow") {
        command = L"nflow('" + escaped + L"')";
        return true;
    }
    command = (method == "open" ? L"edit('" : L"run('") + escaped + L"')";
    return true;
}
//=============================================================================
bool
BuildExamplesCommandFromCatalogCommand(const std::string& method,
    const std::wstring& requestedCommand, const std::vector<std::wstring>& allowedCommands,
    std::wstring& command, std::string& error)
{
    command.clear();
    error.clear();
    // A command entry runs a builtin call; it has no file, so the editor cannot open it.
    if (method != "run") {
        error = "example command cannot be opened in the editor";
        return false;
    }
    const auto found = std::find(allowedCommands.begin(), allowedCommands.end(), requestedCommand);
    if (found == allowedCommands.end()) {
        error = "example command is not in the current catalog";
        return false;
    }
    command = requestedCommand;
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
