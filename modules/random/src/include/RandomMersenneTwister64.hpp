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
#pragma once
//=============================================================================
#include "RandomInterface.hpp"
#include "nlsRandom_exports.h"
#include <boost/container/vector.hpp>
#include <boost/random.hpp>
#include <boost/random/variate_generator.hpp>
#include <string>
//=============================================================================
// http://www.boost.org/doc/libs/1_57_0/doc/html/boost_random/reference.html#boost_random.reference.generators
// http://www.boost.org/doc/libs/1_57_0/doc/html/boost_random/reference.html#boost_random.reference.distributions
namespace Nelson {
class NLSRANDOM_IMPEXP RandomMersenneTwister64 : public RandomInterface
{

private:
    void* generator = nullptr;
    uint64 seed = 0;
    boost::mt19937_64 mersenneTwister64;
    boost::variate_generator<boost::mt19937_64&, boost::uniform_real<>>* uniform_real_generator;
    boost::variate_generator<boost::mt19937_64&, boost::random::uniform_int_distribution<>>*
        uniform_int_generator;
    boost::variate_generator<boost::mt19937_64&, boost::normal_distribution<>>*
        normal_real_generator;

public:
    RandomMersenneTwister64();
    ~RandomMersenneTwister64();

    std::wstring
    getGeneratorName();

    void
    setSeed(uint64 _seed);
    uint64
    getSeed();

    double
    getValueAsDouble(RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL);
    single
    getValueAsSingle(RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL);

    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL);
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL);

    boost::container::vector<uint64>
    getState();
    void
    setState(boost::container::vector<uint64> _state);
    void
    setState(uint64* _state, size_t len);
    size_t
    getStateSize();

    void
    setMinMaxUniformIntDistribution(int _min, int _max);
    void
    getMinMaxUniformIntDistribution(int& _min, int& _max);
};
} // namespace Nelson
//=============================================================================
