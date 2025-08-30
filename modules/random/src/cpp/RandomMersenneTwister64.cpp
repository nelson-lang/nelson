//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4244)
#define _SCL_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <fstream>
#include <iostream>
#include <sstream>
#include "RandomMersenneTwister64.hpp"
//=============================================================================
#define MAGIC_SEED 5489
//=============================================================================
namespace Nelson {
//=============================================================================
RandomMersenneTwister64::RandomMersenneTwister64()
{
    seed = 0;
    setSeed(seed);
}
//=============================================================================
RandomMersenneTwister64::~RandomMersenneTwister64()
{
    uniform_real_generator(true);
    normal_real_generator(true);
}
//=============================================================================
std::wstring
RandomMersenneTwister64::getGeneratorName()
{
    return std::wstring(L"twister64");
}
//=============================================================================
void
RandomMersenneTwister64::setSeed(uint32 _seed)
{
    seed = _seed;
    if (_seed == 0) {
        random_engine().seed(MAGIC_SEED);
    } else {
        random_engine().seed(seed);
    }
}
//=============================================================================
void
RandomMersenneTwister64::setSeed(uint64 _seed)
{
    seed = _seed;
    if (_seed == 0) {
        random_engine().seed(MAGIC_SEED);
    } else {
        random_engine().seed(seed);
    }
}
//=============================================================================
uint64
RandomMersenneTwister64::getSeedU64()
{
    return seed;
}
//=============================================================================
uint32
RandomMersenneTwister64::getSeed()
{
    return (uint32)seed;
}
//=============================================================================
void
RandomMersenneTwister64::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = uniform01();
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                // MATLAB compatible: floor(rand * N) + 1 where N = maxInt
                ar[k * lastDim + l]
                    = static_cast<double>(static_cast<int>(std::floor(uniform01() * maxInt)) + 1);
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = normal();
            }
        }
    } break;
    default: {
    } break;
    }
}
//=============================================================================
void
RandomMersenneTwister64::getValuesAsSingle(
    single* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = static_cast<single>(uniform01());
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                // MATLAB compatible: floor(rand * N) + 1 where N = maxInt
                ar[k * lastDim + l]
                    = static_cast<single>(static_cast<int>(std::floor(uniform01() * maxInt)) + 1);
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = static_cast<single>(normal());
            }
        }
    } break;
    default: {
    } break;
    }
}
//=============================================================================
std::vector<uint64>
RandomMersenneTwister64::getState()
{
    std::vector<uint64> state;
    std::stringstream line;
    line << random_engine();
    uint64 num = 0;
    while (line >> num) {
        state.push_back(num);
    }
    return state;
}
//=============================================================================
void
RandomMersenneTwister64::setState(const uint32* _state, size_t len)
{
    std::stringstream line;
    for (size_t k = 0; k < len; k++) {
        line << ' ' << _state[k];
    }
    line >> random_engine();
}
//=============================================================================
void
RandomMersenneTwister64::setState(const uint64* _state, size_t len)
{
    std::stringstream line;
    for (size_t k = 0; k < len; k++) {
        line << ' ' << _state[k];
    }
    line >> random_engine();
}
//=============================================================================
size_t
RandomMersenneTwister64::getStateSize()
{
    return getState().size();
}
//=============================================================================
void
RandomMersenneTwister64::setMinMaxUniformIntDistribution(int _min, int _max)
{
    minInt = _min;
    maxInt = _max;
}
//=============================================================================
void
RandomMersenneTwister64::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    _min = minInt;
    _max = maxInt;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
