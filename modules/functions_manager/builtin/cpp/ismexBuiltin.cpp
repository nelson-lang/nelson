//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismexBuiltin.hpp"
#include "characters_encoding.hpp"
#include "PathFuncManager.hpp"
#include "MxGetExtension.hpp"
#include "StringHelpers.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::ismexBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    std::wstring name = param1.getContentAsWideString();
    std::string uname = wstring_to_utf8(name);
    wstringVector filenames;
    bool res = false;
    PathFuncManager::getInstance()->find(name, filenames);
    if (!filenames.empty()) {
        for (const auto& filename : filenames) {
            if (StringHelpers::ends_with(filename, getMexExtension())) {
                res = true;
                break;
            }
        }
    }
    retval << ArrayOf::logicalConstructor(res);
    return retval;
}
//=============================================================================
