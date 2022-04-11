//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    ~RandomMersenneTwister64() override;

    std::wstring
    getGeneratorName() override;

    void
    setSeed(uint64 _seed);
    uint64
    getSeed();

    double
    getValueAsDouble(RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;
    single
    getValueAsSingle(RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;

    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;

    boost::container::vector<uint64>
    getState();
    void
    setState(const boost::container::vector<uint64>& _state);
    void
    setState(uint64* _state, size_t len);
    size_t
    getStateSize() override;

    void
    setMinMaxUniformIntDistribution(int _min, int _max) override;
    void
    getMinMaxUniformIntDistribution(int& _min, int& _max) override;
};
} // namespace Nelson
//=============================================================================
