//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <iostream>
#include <string>
#include "__nelson_unittest_worker_receive__Builtin.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_worker_receive__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    std::string line;
    if (!std::getline(std::cin, line)) {
        line.clear();
    }
    if (!line.empty() && line.back() == '\r') {
        line.pop_back();
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(utf8_to_wstring(line));
    return retval;
}
//=============================================================================
