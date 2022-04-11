//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include <algorithm>
#include <cstdlib>
#include "fast_float/fast_float.h"
#include "AsciiToDouble.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
asciiToDouble(const std::string& str)
{
    double value;
    auto answer = fast_float::from_chars(str.data(), str.data() + str.size(), value);
    return value;
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
