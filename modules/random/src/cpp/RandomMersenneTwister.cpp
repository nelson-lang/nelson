//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "RandomMersenneTwister.hpp"
#include <boost/random/seed_seq.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
//=============================================================================
#define MAGIC_SEED 5489
//=============================================================================
namespace Nelson {
//=============================================================================
RandomMersenneTwister::RandomMersenneTwister()
{
    seed = 0;
    uniform_real_generator = new boost::variate_generator<boost::mt19937&, boost::uniform_real<>>(
        mersenneTwister, boost::uniform_real<>(0., 1.));
    normal_real_generator
        = new boost::variate_generator<boost::mt19937&, boost::normal_distribution<>>(
            mersenneTwister, boost::normal_distribution<>(0., 1.));
    uniform_int_generator
        = new boost::variate_generator<boost::mt19937&, boost::random::uniform_int_distribution<>>(
            mersenneTwister, boost::random::uniform_int_distribution<>(1, 1000));
    setSeed(seed);
}
//=============================================================================
RandomMersenneTwister::~RandomMersenneTwister()
{
    if (uniform_real_generator) {
        delete uniform_real_generator;
        uniform_real_generator = nullptr;
    }
    if (uniform_int_generator) {
        delete uniform_int_generator;
        uniform_int_generator = nullptr;
    }
    if (normal_real_generator) {
        delete normal_real_generator;
        normal_real_generator = nullptr;
    }
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
        mersenneTwister.seed(MAGIC_SEED);
    } else {
        mersenneTwister.seed(seed);
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
        return (single)(*uniform_real_generator)();
    case RNG_DISTRIBUTION_UNIFORM_INT:
        return (single)(*uniform_int_generator)();
    case RNG_DISTRIBUTION_NORMAL:
        return (single)(*normal_real_generator)();
    default: { } break; }
    return static_cast<single>(nan(""));
}
//=============================================================================
double
RandomMersenneTwister::getValueAsDouble(RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL:
        return (*uniform_real_generator)();
    case RNG_DISTRIBUTION_UNIFORM_INT:
        return (double)(*uniform_int_generator)();
    case RNG_DISTRIBUTION_NORMAL:
        return (*normal_real_generator)();
    default: { } break; }
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
                ar[k * lastDim + l] = (*uniform_real_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_real_generator)();
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (double)(*uniform_int_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_int_generator)();
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (*normal_real_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*normal_real_generator)();
            }
        }
    } break;
    default: { } break; }
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
                ar[k * lastDim + l] = (single)(*uniform_real_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_real_generator)();
            }
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (single)(*uniform_int_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*uniform_int_generator)();
            }
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        // rows to columns order
        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (single)(*normal_real_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*normal_real_generator)();
            }
        }
    } break;
    default: { } break; }
}
//=============================================================================
boost::container::vector<uint32>
RandomMersenneTwister::getState()
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    boost::container::vector<uint32> state;
    std::stringstream line;
    line << mersenneTwister;
    uint32 num = 0;
    while (line >> num) {
        state.push_back(num);
    }
    return state;
}
//=============================================================================
void
RandomMersenneTwister::setState(const boost::container::vector<uint32>& _state)
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
    line >> mersenneTwister;
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
    line >> mersenneTwister;
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
    if (uniform_int_generator) {
        delete uniform_int_generator;
    }
    uniform_int_generator
        = new boost::variate_generator<boost::mt19937&, boost::random::uniform_int_distribution<>>(
            mersenneTwister, boost::random::uniform_int_distribution<>(_min, _max));
}
//=============================================================================
void
RandomMersenneTwister::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    _max = uniform_int_generator->distribution().max();
    _min = uniform_int_generator->distribution().min();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
