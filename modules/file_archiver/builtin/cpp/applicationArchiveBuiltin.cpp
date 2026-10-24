//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "applicationArchiveBuiltin.hpp"
#include "ApplicationArchive.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <stdexcept>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FileArchiverGateway::applicationArchiveBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 6);
    nargoutcheck(nLhs, 0, 0);
    const auto operation = argIn[0].asWideString();
    try {
        if (operation == L"verify") {
            nargincheck(argIn, 2, 2);
            verifyApplicationArchive(argIn[1].asWideString());
        } else if (operation == L"extract") {
            nargincheck(argIn, 3, 3);
            extractApplicationArchive(argIn[1].asWideString(), argIn[2].asWideString());
        } else if (operation == L"directory") {
            nargincheck(argIn, 2, 2);
            createApplicationDirectory(argIn[1].asWideString());
        } else {
            throw std::runtime_error("Unknown application archive operation.");
        }
    } catch (const std::exception& error) {
        RaiseError(error.what(), "Nelson:nelsonc:archive");
    }
    return {};
}
//=============================================================================
