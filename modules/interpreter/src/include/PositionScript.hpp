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
    [[nodiscard]] bool
    isDirectScript() const
    {
        return this->directScript;
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
