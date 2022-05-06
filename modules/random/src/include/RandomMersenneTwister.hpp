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
#include <boost/container/vector.hpp>
#include <boost/random.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/random/variate_generator.hpp>
#include <string>
#include "RandomInterface.hpp"
#include "nlsRandom_exports.h"
//=============================================================================
// http://www.boost.org/doc/libs/1_57_0/doc/html/boost_random/reference.html#boost_random.reference.generators
// http://www.boost.org/doc/libs/1_57_0/doc/html/boost_random/reference.html#boost_random.reference.distributions
//=============================================================================
namespace Nelson {
class NLSRANDOM_IMPEXP RandomMersenneTwister : public RandomInterface
{

private:
    uint32 seed = 0;
    boost::mt19937 mersenneTwister;
    boost::variate_generator<boost::mt19937&, boost::uniform_real<>>* uniform_real_generator;
    boost::variate_generator<boost::mt19937&, boost::random::uniform_int_distribution<>>*
        uniform_int_generator;
    boost::variate_generator<boost::mt19937&, boost::normal_distribution<>>* normal_real_generator;

public:
    RandomMersenneTwister();
    ~RandomMersenneTwister() override;

    std::wstring
    getGeneratorName() override;

    void
    setSeed(uint32 _seed);
    uint32
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

    boost::container::vector<uint32>
    getState();
    void
    setState(const boost::container::vector<uint32>& _state);
    void
    setState(uint32* _state, size_t len);
    size_t
    getStateSize() override;

    void
    setMinMaxUniformIntDistribution(int _min, int _max) override;
    void
    getMinMaxUniformIntDistribution(int& _min, int& _max) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
