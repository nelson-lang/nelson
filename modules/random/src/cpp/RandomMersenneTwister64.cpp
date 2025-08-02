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
#include <boost/random/seed_seq.hpp>
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
    uniform_int_generator(true);
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
RandomMersenneTwister64::getSeed()
{
    return seed;
}
//=============================================================================
single
RandomMersenneTwister64::getValueAsSingle(RNG_DISTRIBUTION_TYPE _type)
{
    if (generator == &uniform_real_generator()) {
        return (single)(*uniform_real_generator())();
    }
    return static_cast<single>(nan(""));
}
//=============================================================================
double
RandomMersenneTwister64::getValueAsDouble(RNG_DISTRIBUTION_TYPE _type)
{
    if (generator == &uniform_real_generator()) {
        return (*uniform_real_generator())();
    }
    return nan("");
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
                ar[k * lastDim + l] = (*uniform_real_generator())();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_real_generator())();
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (double)(*uniform_int_generator())();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_int_generator())();
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (double)(*normal_real_generator())();
                // We reject voluntary the next random value for simulate complex number array
                (*normal_real_generator())();
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
                ar[k * lastDim + l] = (single)(*uniform_real_generator())();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_real_generator())();
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (single)(*uniform_int_generator())();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_int_generator())();
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (single)(*normal_real_generator())();
                // We reject voluntary the next random value for simulate complex number array
                (*normal_real_generator())();
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
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
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
RandomMersenneTwister64::setState(const std::vector<uint64>& _state)
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    std::stringstream line;
    for (uint64 k : _state) {
        line << ' ' << (unsigned long)k;
    }
    line >> random_engine();
}
//=============================================================================
void
RandomMersenneTwister64::setState(uint64* _state, size_t len)
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
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
    uniform_int_generator(true);
    uniform_int_generator(false, _min, _max);
}
//=============================================================================
void
RandomMersenneTwister64::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    _max = uniform_int_generator()->distribution().max();
    _min = uniform_int_generator()->distribution().min();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
