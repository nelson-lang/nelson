//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_addlibrarypathBuiltin.hpp"
#include "BlockRegistry.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <filesystem>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace fs = std::filesystem;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
static bool
samePath(const std::string& a, const std::string& b);
//=============================================================================
// NFlow.addlibrarypath(folder) adds a directory to the BlockRegistry search
// paths and rescans, so a freshly built external block library becomes visible
// to the simulation engine, the code generator and the editor.
//=============================================================================
ArrayOfVector
nflow_addlibrarypathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    fs::path dir(argIn[0].asWideString());
    std::error_code ec;
    if (!fs::is_directory(dir, ec)) {
        Error(L"Nelson:nflow_engine:libraryPathNotADirectory",
            _W("A valid existing directory is expected."));
    }
    const std::string folder = argIn[0].asUtf8String();
    auto& reg = BlockRegistry::instance();
    stringVector paths = reg.searchPaths();
    bool already = false;
    for (const auto& p : paths) {
        if (samePath(p, folder)) {
            already = true;
            break;
        }
    }
    if (!already) {
        paths.push_back(folder);
        reg.setSearchPaths(paths);
    }
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(reg.searchPaths());
    }
    return retval;
}
//=============================================================================
static bool
samePath(const std::string& a, const std::string& b)
{
    return fs::path(a).lexically_normal() == fs::path(b).lexically_normal();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
