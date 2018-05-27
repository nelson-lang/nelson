//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Rng_helpers.hpp"

//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
getSupportedRngEngineName()
{
    wstringVector supportedEngineName;
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
}
//=============================================================================
