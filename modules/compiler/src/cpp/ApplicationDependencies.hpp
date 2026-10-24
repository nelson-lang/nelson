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
#include <filesystem>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
class MacroFunctionDef;
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace Nelson::Application {
inline void
require(bool valid, const char* message)
{
    if (!valid) {
        throw std::runtime_error(message);
    }
}
//=============================================================================
struct Reference
{
    std::string name;
    std::string kind;
    int line = 0;
    std::vector<std::string> imports;
    bool loadFilename = false;
};
//=============================================================================
struct SourceDependencies
{
    bool script = false;
    bool sourceRequired = false;
    std::set<std::string> methods;
    std::set<std::string> definitions;
    std::vector<Reference> references;
};
//=============================================================================
SourceDependencies
analyzeSource(const std::filesystem::path& source);
std::vector<Reference>
readSourceDirectives(const std::filesystem::path& source);
void
analyzeFunctions(SourceDependencies& result, MacroFunctionDef& entry);
SourceDependencies
analyzeClassSource(const std::filesystem::path& source);
void
retainConstructorMethods(SourceDependencies& result, const std::filesystem::path& source);
std::string
digestFile(const std::filesystem::path& path);
//=============================================================================
} // namespace Nelson::Application
//=============================================================================
