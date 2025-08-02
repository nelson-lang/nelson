//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ConvertStringsToChars.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "convertStringsToCharsBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::convertStringsToCharsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (nLhs > (int)argIn.size()) {
        Error(_W("Number of Input arguments must the same as output."));
    }
    return ConvertStringsToChars(argIn);
}
//=============================================================================
