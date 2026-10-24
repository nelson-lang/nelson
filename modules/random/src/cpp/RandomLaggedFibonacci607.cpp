//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include <fstream>
#include <iostream>
#include <sstream>
#include <random>
#include "RandomLaggedFibonacci607.hpp"
#include "LaggedFibonacci607Engine.hpp"
//=============================================================================
namespace Nelson {
RandomLaggedFibonacci607::RandomLaggedFibonacci607()
{
    seed = 0;
    setSeed(seed);
}
//=============================================================================
RandomLaggedFibonacci607::~RandomLaggedFibonacci607() = default;
//=============================================================================
std::wstring
RandomLaggedFibonacci607::getGeneratorName()
{
    return std::wstring(L"laggedfibonacci607");
}
//=============================================================================
void
RandomLaggedFibonacci607::setSeed(uint32 _seed)
{
    seed = _seed;
    random_engine().seed(seed);
}
//=============================================================================
uint32
RandomLaggedFibonacci607::getSeed()
{
    return seed;
}
//=============================================================================
void
RandomLaggedFibonacci607::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = uniform01();
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = static_cast<double>(minInt
                + static_cast<int64>(
                    std::floor(uniform01() * static_cast<double>(maxInt - minInt + 1))));
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = normal();
            k++;
        }
    } break;
    default: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = nan("");
            k++;
        }
    } break;
    }
}
//=============================================================================
void
RandomLaggedFibonacci607::getValuesAsSingle(
    single* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = static_cast<single>(uniform01());
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = static_cast<single>(minInt
                + static_cast<int64>(
                    std::floor(uniform01() * static_cast<double>(maxInt - minInt + 1))));
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = static_cast<single>(normal());
            k++;
        }
    } break;
    default: {
        indexType k = 0;
        while (k < nbElements) {
            ar[k] = static_cast<single>(nan(""));
            k++;
        }
    } break;
    }
}
//=============================================================================
std::vector<uint32>
RandomLaggedFibonacci607::getState()
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    std::vector<uint32> state;
    std::stringstream line;
    line << random_engine();
    uint32 num = 0;
    while (line >> num) {
        state.push_back(num);
    }
    return state;
}
//=============================================================================
void
RandomLaggedFibonacci607::setState(const uint32* _state, size_t len)
{
    std::stringstream line;
    for (size_t k = 0; k < len; k++) {
        line << ' ' << _state[k];
    }
    line >> random_engine();
}
//=============================================================================
size_t
RandomLaggedFibonacci607::getStateSize()
{
    return getState().size();
}
//=============================================================================
void
RandomLaggedFibonacci607::setMinMaxUniformIntDistribution(int64 _min, int64 _max)
{
    minInt = _min;
    maxInt = _max;
}
//=============================================================================
void
RandomLaggedFibonacci607::getMinMaxUniformIntDistribution(int64& _min, int64& _max)
{
    _min = minInt;
    _max = maxInt;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
