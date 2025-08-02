//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Rng_helpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
getSupportedRngEngineName()
{
    wstringVector supportedEngineName;
    supportedEngineName.reserve(3);
    supportedEngineName.push_back(L"twister");
    supportedEngineName.push_back(L"twister64");
    supportedEngineName.push_back(L"laggedfibonacci607");
    return supportedEngineName;
}
//=============================================================================
std::wstring
getRngTypeAsString(RNG_TYPE rngType)
{
    std::wstring res;
    switch (rngType) {
    case RNG_TWISTER: {
        res = L"twister";
    } break;
    case RNG_TWISTER64: {
        res = L"twister64";
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        res = L"laggedfibonacci607";
    } break;
    case RNG_ERROR:
    default:
        break;
    }
    return res;
}
//=============================================================================
RNG_TYPE
getRngType(const std::wstring& enginename)
{
    RNG_TYPE res = RNG_ERROR;
    if (enginename == L"twister") {
        return RNG_TWISTER;
    }
    if (enginename == L"twister64") {
        return RNG_TWISTER64;
    }
    if (enginename == L"laggedfibonacci607") {
        return RNG_LAGGED_FIBONACCI_607;
    }
    return res;
}
//=============================================================================
bool
isRngType(const std::wstring& enginename)
{
    RNG_TYPE res = getRngType(enginename);
    return (res != RNG_ERROR);
}
//=============================================================================
bool
haveRandomEngine()
{
    return (NelsonConfiguration::getInstance()->getRandomEngine() != nullptr);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
