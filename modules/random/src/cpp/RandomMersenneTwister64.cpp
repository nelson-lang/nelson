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
RandomMersenneTwister64::~RandomMersenneTwister64() = default;
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
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = uniform01();
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                // imin + floor(rand * (imax - imin + 1))
                ar[k * lastDim + l] = static_cast<double>(minInt
                    + static_cast<int64>(
                        std::floor(uniform01() * static_cast<double>(maxInt - minInt + 1))));
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
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
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = static_cast<single>(uniform01());
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                // imin + floor(rand * (imax - imin + 1))
                ar[k * lastDim + l] = static_cast<single>(minInt
                    + static_cast<int64>(
                        std::floor(uniform01() * static_cast<double>(maxInt - minInt + 1))));
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
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
    std::vector<uint64> state(MersenneTwister64Engine::stateSize);
    random_engine().getState(state.data());
    return state;
}
//=============================================================================
void
RandomMersenneTwister64::setState(const uint32* _state, size_t len)
{
    if (len == MersenneTwister64Engine::stateSize) {
        random_engine().setState(_state);
    }
}
//=============================================================================
void
RandomMersenneTwister64::setState(const uint64* _state, size_t len)
{
    if (len == MersenneTwister64Engine::stateSize) {
        random_engine().setState(_state);
    }
}
//=============================================================================
size_t
RandomMersenneTwister64::getStateSize()
{
    return MersenneTwister64Engine::stateSize;
}
//=============================================================================
void
RandomMersenneTwister64::setMinMaxUniformIntDistribution(int64 _min, int64 _max)
{
    minInt = _min;
    maxInt = _max;
}
//=============================================================================
void
RandomMersenneTwister64::getMinMaxUniformIntDistribution(int64& _min, int64& _max)
{
    _min = minInt;
    _max = maxInt;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
