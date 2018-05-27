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
#ifdef _MSC_VER
#pragma warning(disable : 4244)
#define _SCL_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "RandomMersenneTwister64.hpp"
#include <boost/random/seed_seq.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
RandomMersenneTwister64::RandomMersenneTwister64()
{
    seed = 0;
    uniform_real_generator
        = new boost::variate_generator<boost::mt19937_64&, boost::uniform_real<>>(
            mersenneTwister64, boost::uniform_real<>(0., 1.));
    normal_real_generator
        = new boost::variate_generator<boost::mt19937_64&, boost::normal_distribution<>>(
            mersenneTwister64, boost::normal_distribution<>(0., 1.));
    uniform_int_generator = new boost::variate_generator<boost::mt19937_64&,
        boost::random::uniform_int_distribution<>>(
        mersenneTwister64, boost::random::uniform_int_distribution<>(1, 1000));
    setSeed(seed);
}
//=============================================================================
RandomMersenneTwister64::~RandomMersenneTwister64()
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
RandomMersenneTwister64::getGeneratorName()
{
    return std::wstring(L"twister64");
}
//=============================================================================
void
RandomMersenneTwister64::setSeed(uint64 _seed)
{
    seed = _seed;
    mersenneTwister64.seed(seed);
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
    if (generator == &uniform_real_generator) {
        return (single)(*uniform_real_generator)();
    }
    return (single)nan("");
}
//=============================================================================
double
RandomMersenneTwister64::getValueAsDouble(RNG_DISTRIBUTION_TYPE _type)
{
    if (generator == &uniform_real_generator) {
        return (*uniform_real_generator)();
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
            for (size_t l = 0; l < lastDim; l++) {
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
            for (size_t l = 0; l < lastDim; l++) {
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
            for (size_t l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (double)(*normal_real_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*normal_real_generator)();
            }
        }
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
            for (size_t l = 0; l < lastDim; l++) {
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
            for (size_t l = 0; l < lastDim; l++) {
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
            for (size_t l = 0; l < lastDim; l++) {
                ar[k * lastDim + l] = (single)(*normal_real_generator)();
                // We reject voluntary the next random value for simulate complex number array
                (*normal_real_generator)();
            }
        }
    } break;
    }
}
//=============================================================================
boost::container::vector<uint64>
RandomMersenneTwister64::getState()
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    boost::container::vector<uint64> state;
    std::stringstream line;
    line << mersenneTwister64;
    uint64 num = 0;
    while (line >> num) {
        state.push_back(num);
    }
    return state;
}
//=============================================================================
void
RandomMersenneTwister64::setState(boost::container::vector<uint64> _state)
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    std::stringstream line;
    for (size_t k = 0; k < _state.size(); k++) {
        line << ' ' << _state[k];
    }
    line >> mersenneTwister64;
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
    line >> mersenneTwister64;
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
    if (uniform_int_generator) {
        delete uniform_int_generator;
    }
    uniform_int_generator = new boost::variate_generator<boost::mt19937_64&,
        boost::random::uniform_int_distribution<>>(
        mersenneTwister64, boost::random::uniform_int_distribution<>(_min, _max));
}
//=============================================================================
void
RandomMersenneTwister64::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    _max = uniform_int_generator->distribution().max();
    _min = uniform_int_generator->distribution().min();
}
//=============================================================================
}
//=============================================================================
