//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_rmlibrarypathBuiltin.hpp"
#include "BlockRegistry.hpp"
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
// NFlow.rmlibrarypath(folder) removes a directory from the BlockRegistry search
// paths and rescans the remaining ones.
//=============================================================================
ArrayOfVector
nflow_rmlibrarypathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    const std::string folder = argIn[0].asUtf8String();
    auto& reg = BlockRegistry::instance();
    stringVector paths = reg.searchPaths();
    stringVector kept;
    kept.reserve(paths.size());
    for (const auto& p : paths) {
        if (!samePath(p, folder)) {
            kept.push_back(p);
        }
    }
    if (kept.size() != paths.size()) {
        reg.setSearchPaths(kept);
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
