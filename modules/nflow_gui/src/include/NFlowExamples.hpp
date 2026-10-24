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
#include <vector>
#include "nlsNflow_gui_exports.h"
//=============================================================================
namespace Nelson::NFlowExamples {
//=============================================================================
// Enumerates the runnable nflow examples and returns them as a JSON string:
//   { "examples": [ { path, name, description, author, kind, category, group,
//                     source, tags[], blockCount, runtime, sampleTime, mtime },
//                   ... ] }
// Built-in roots are the nflow_blocks/examples (one category per sub-directory)
// and nflow_fmi/examples trees, resolved through ModulePath. External roots are
// any "examples" directory found under a registered library search path (either
// directly at the search path, or inside a library sub-directory carrying a
// library.json). The caller supplies the library search paths and the discovered
// library manifests (each a JSON string with "_dir" / "title") so this unit does
// not depend on the block registry.
//=============================================================================
NLSNFLOW_GUI_IMPEXP std::string
listExamplesJson(const std::vector<std::string>& librarySearchPaths,
    const std::vector<std::string>& libraryManifests);
//=============================================================================
} // namespace Nelson::NFlowExamples
//=============================================================================
