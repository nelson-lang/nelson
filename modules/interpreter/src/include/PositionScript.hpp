//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <utility>
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class PositionScript
{
    //=============================================================================
private:
    std::wstring filename = L"";
    std::wstring functionname = L"";
    int line = -1;
    bool directScript = false;
    //=============================================================================
public:
    //=============================================================================
    PositionScript(std::wstring functionname = L"", std::wstring filename = L"", int line = -1,
        bool isDirectScript = false)
        : filename(std::move(filename))
        , functionname(std::move(functionname))
        , line(line)
        , directScript(isDirectScript)
    {
    }
    //=============================================================================
    PositionScript(const PositionScript& copy)
    {
        this->filename = copy.filename;
        this->functionname = copy.functionname;
        this->line = copy.line;
        this->directScript = copy.directScript;
    }
    //=============================================================================
    void
    operator=(const PositionScript& copy)
    {
        if (this == &copy) {
            return;
        }
        this->filename = copy.filename;
        this->functionname = copy.functionname;
        this->line = copy.line;
        this->directScript = copy.directScript;
    }
    //=============================================================================
    ~PositionScript()
    {
        this->filename.clear();
        this->functionname.clear();
        this->line = -1;
        this->directScript = false;
    }
    //=============================================================================
    [[nodiscard]] const std::wstring&
    getFilename() const
    {
        return this->filename;
    }
    //=============================================================================
    void
    setFilename(const std::wstring& scriptFilename)
    {
        this->filename = scriptFilename;
    }
    //=============================================================================
    [[nodiscard]] int
    getLine() const
    {
        return this->line;
    }
    //=============================================================================
    void
    setFunctionName(const std::wstring& scriptFunctionName)
    {
        this->functionname = scriptFunctionName;
    }
    //=============================================================================
    [[nodiscard]] const std::wstring&
    getFunctionName() const
    {
        return this->functionname;
    }
    //=============================================================================
    // Name shown in stack displays: file>local for a local function (the form dbstop takes),
    // else the name itself (main function, script, method, package or anonymous function).
    [[nodiscard]] std::wstring
    getDisplayName() const
    {
        const size_t slash = this->filename.find_last_of(L"/\\");
        std::wstring stem
            = slash == std::wstring::npos ? this->filename : this->filename.substr(slash + 1);
        const size_t dot = stem.rfind(L'.');
        if (dot != std::wstring::npos) {
            stem.resize(dot);
        }
        if (stem.empty() || this->functionname.empty() || this->functionname == stem
            || this->functionname.find_first_of(L">.@/") != std::wstring::npos) {
            return this->functionname.empty() ? stem : this->functionname;
        }
        return stem + L">" + this->functionname;
    }
    //=============================================================================
    [[nodiscard]] bool
    isDirectScript() const
    {
        return this->directScript;
    }
    //=============================================================================
    // Frames of compiled functions (error, zeros, cellfun...) are not shown in stacks; run is
    // kept, the scripts it runs being listed below it.
    [[nodiscard]] bool
    isShownInStacks() const
    {
        return !this->filename.empty() || this->line > 0 || this->functionname == L"run";
    }
    //=============================================================================
    [[nodiscard]] bool
    isEmpty() const
    {
        return (this->functionname.empty() && this->filename.empty() && this->line == -1);
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
