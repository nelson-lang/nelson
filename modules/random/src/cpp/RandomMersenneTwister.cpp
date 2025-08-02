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
#include <boost/random/seed_seq.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include "RandomMersenneTwister.hpp"
//=============================================================================
#define MAGIC_SEED 5489
//=============================================================================
namespace Nelson {
//=============================================================================
RandomMersenneTwister::RandomMersenneTwister()
{
    seed = 0;
    setSeed(seed);
}
//=============================================================================
RandomMersenneTwister::~RandomMersenneTwister()
{
    uniform_real_generator(true);
    uniform_int_generator(true);
    normal_real_generator(true);
}
//=============================================================================
std::wstring
RandomMersenneTwister::getGeneratorName()
{
    return std::wstring(L"twister");
}
//=============================================================================
void
RandomMersenneTwister::setSeed(uint32 _seed)
{
    seed = _seed;
    if (_seed == 0) {
        random_engine().seed(MAGIC_SEED);
    } else {
        random_engine().seed(seed);
    }
}
//=============================================================================
uint32
RandomMersenneTwister::getSeed()
{
    return seed;
}
//=============================================================================
single
RandomMersenneTwister::getValueAsSingle(RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL:
        return (single)(*uniform_real_generator())();
    case RNG_DISTRIBUTION_UNIFORM_INT:
        return (single)(*uniform_int_generator())();
    case RNG_DISTRIBUTION_NORMAL:
        return (single)(*normal_real_generator())();
    default: {
    } break;
    }
    return static_cast<single>(nan(""));
}
//=============================================================================
double
RandomMersenneTwister::getValueAsDouble(RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL:
        return (*uniform_real_generator())();
    case RNG_DISTRIBUTION_UNIFORM_INT:
        return (double)(*uniform_int_generator())();
    case RNG_DISTRIBUTION_NORMAL:
        return (*normal_real_generator())();
    default: {
    } break;
    }
    return nan("");
}
//=============================================================================
void
RandomMersenneTwister::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (*uniform_real_generator(false))();
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
                ar[k * lastDim + l] = (*normal_real_generator())();
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
RandomMersenneTwister::getValuesAsSingle(
    single* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (single)(*uniform_real_generator(false))();
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
std::vector<uint32>
RandomMersenneTwister::getState()
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
RandomMersenneTwister::setState(const std::vector<uint32>& _state)
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    std::stringstream line;
    for (size_t k = 0; k < _state.size(); k++) {
        if (k == 0) {
            line << _state[k];
        } else {
            line << ' ' << _state[k];
        }
    }
    line >> random_engine();
}
//=============================================================================
void
RandomMersenneTwister::setState(uint32* _state, size_t len)
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    std::stringstream line;
    for (size_t k = 0; k < len; k++) {
        if (k == 0) {
            line << _state[k];
        } else {
            line << ' ' << _state[k];
        }
    }
    line >> random_engine();
}
//=============================================================================
size_t
RandomMersenneTwister::getStateSize()
{
    return getState().size();
}
//=============================================================================
void
RandomMersenneTwister::setMinMaxUniformIntDistribution(int _min, int _max)
{
    uniform_int_generator(true);
    uniform_int_generator(false, _min, _max);
}
//=============================================================================
void
RandomMersenneTwister::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    _max = uniform_int_generator()->distribution().max();
    _min = uniform_int_generator()->distribution().min();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
