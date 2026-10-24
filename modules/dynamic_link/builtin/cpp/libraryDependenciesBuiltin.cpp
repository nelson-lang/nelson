//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libraryDependenciesBuiltin.hpp"
#include "LibraryDependencies.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include <stdexcept>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libraryDependenciesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    try {
        return { ArrayOf::characterArray(inspectLibraryDependencies(argIn[0].asWideString())) };
    } catch (const std::exception& error) {
        RaiseError(error.what(), "Nelson:nelsonc:nativeLibrary");
    }
    return {};
}
//=============================================================================
