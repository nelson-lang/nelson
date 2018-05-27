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
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "RandomLaggedFibonacci607.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
//=============================================================================
namespace Nelson {
RandomLaggedFibonacci607::RandomLaggedFibonacci607()
{
    seed = 0;
    uniform_real_generator
        = new boost::variate_generator<boost::lagged_fibonacci607&, boost::uniform_real<>>(
            rngLaggedFibonacci607, boost::uniform_real<>(0., 1.));
    normal_real_generator
        = new boost::variate_generator<boost::lagged_fibonacci607&, boost::normal_distribution<>>(
            rngLaggedFibonacci607, boost::normal_distribution<>(0., 1.));
    uniform_int_generator = new boost::variate_generator<boost::lagged_fibonacci607&,
        boost::random::uniform_int_distribution<>>(
        rngLaggedFibonacci607, boost::random::uniform_int_distribution<>(1, 1000));
    setSeed(seed);
}
//=============================================================================
RandomLaggedFibonacci607::~RandomLaggedFibonacci607()
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
RandomLaggedFibonacci607::getGeneratorName()
{
    return std::wstring(L"laggedfibonacci607");
}
//=============================================================================
void
RandomLaggedFibonacci607::setSeed(uint32 _seed)
{
    seed = _seed;
    rngLaggedFibonacci607.seed(seed);
}
//=============================================================================
uint32
RandomLaggedFibonacci607::getSeed()
{
    return seed;
}
//=============================================================================
double
RandomLaggedFibonacci607::getValueAsDouble(RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL:
        return (*uniform_real_generator)();
    case RNG_DISTRIBUTION_UNIFORM_INT:
        return (double)(*uniform_int_generator)();
    case RNG_DISTRIBUTION_NORMAL:
        return (*normal_real_generator)();
    }
    return nan("");
}
//=============================================================================
single
RandomLaggedFibonacci607::getValueAsSingle(RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL:
        return (single)(*uniform_real_generator)();
    case RNG_DISTRIBUTION_UNIFORM_INT:
        return (single)(*uniform_int_generator)();
    case RNG_DISTRIBUTION_NORMAL:
        return (single)(*normal_real_generator)();
    }
    return (single)nan("");
}
//=============================================================================
void
RandomLaggedFibonacci607::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    switch (_type) {
    case RNG_DISTRIBUTION_UNIFORM_REAL: {
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (*uniform_real_generator)();
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (double)(*uniform_int_generator)();
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (*normal_real_generator)();
            k++;
        }
    } break;
    default: {
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (double)nan("");
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
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (single)(*uniform_real_generator)();
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_UNIFORM_INT: {
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (single)(*uniform_int_generator)();
            k++;
        }
    } break;
    case RNG_DISTRIBUTION_NORMAL: {
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (single)(*normal_real_generator)();
            k++;
        }
    } break;
    default: {
        size_t k = 0;
        while (k < nbElements) {
            ar[k] = (single)nan("");
            k++;
        }
    } break;
    }
}
//=============================================================================
boost::container::vector<uint32>
RandomLaggedFibonacci607::getState()
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    boost::container::vector<uint32> state;
    std::stringstream line;
    line << rngLaggedFibonacci607;
    uint32 num = 0;
    while (line >> num) {
        state.push_back(num);
    }
    return state;
}
//=============================================================================
void
RandomLaggedFibonacci607::setState(boost::container::vector<uint32> _state)
{
    // http://www.bnikolic.co.uk/nqm/random/mersenne-boost.html
    std::stringstream line;
    for (size_t k = 0; k < _state.size(); k++) {
        line << ' ' << _state[k];
    }
    line >> rngLaggedFibonacci607;
}
//=============================================================================
void
RandomLaggedFibonacci607::setState(uint32* _state, size_t len)
{
    std::stringstream line;
    for (size_t k = 0; k < len; k++) {
        line << ' ' << _state[k];
    }
    line >> rngLaggedFibonacci607;
}
//=============================================================================
size_t
RandomLaggedFibonacci607::getStateSize()
{
    return getState().size();
}
//=============================================================================
void
RandomLaggedFibonacci607::setMinMaxUniformIntDistribution(int _min, int _max)
{
    if (uniform_int_generator) {
        delete uniform_int_generator;
    }
    uniform_int_generator = new boost::variate_generator<boost::lagged_fibonacci607&,
        boost::random::uniform_int_distribution<>>(
        rngLaggedFibonacci607, boost::random::uniform_int_distribution<>(_min, _max));
}
//=============================================================================
void
RandomLaggedFibonacci607::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    _max = uniform_int_generator->distribution().max();
    _min = uniform_int_generator->distribution().min();
}
//=============================================================================
}
//=============================================================================
