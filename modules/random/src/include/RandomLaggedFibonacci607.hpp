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
#include <boost/random/variate_generator.hpp>
#include <string>
#include "RandomInterface.hpp"
#include "nlsRandom_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSRANDOM_IMPEXP RandomLaggedFibonacci607 : public RandomInterface
{

private:
    void* generator = nullptr;
    uint32 seed = 0;
    auto&
    random_engine()
    {
        thread_local static boost::lagged_fibonacci607 rngLaggedFibonacci607;
        return rngLaggedFibonacci607;
    }
    auto&
    uniform_real_generator(bool doDelete = false)
    {
        thread_local static boost::variate_generator<boost::lagged_fibonacci607&,
            boost::uniform_real<>>* uniform_real_generator
            = nullptr;
        if (doDelete) {
            delete uniform_real_generator;
            uniform_real_generator = nullptr;
            return uniform_real_generator;
        }
        if (uniform_real_generator == nullptr) {
            uniform_real_generator
                = new boost::variate_generator<boost::lagged_fibonacci607&, boost::uniform_real<>>(
                    random_engine(), boost::uniform_real<>(0., 1.));
        }
        return uniform_real_generator;
    }

    auto&
    uniform_int_generator(bool doDelete = false, int _min = 1, int _max = 1000)
    {
        thread_local static boost::variate_generator<boost::lagged_fibonacci607&,
            boost::random::uniform_int_distribution<>>* uniform_int_generator;
        if (doDelete) {
            delete uniform_int_generator;
            uniform_int_generator = nullptr;
            return uniform_int_generator;
        }
        if (uniform_int_generator == nullptr) {
            uniform_int_generator = new boost::variate_generator<boost::lagged_fibonacci607&,
                boost::random::uniform_int_distribution<>>(
                random_engine(), boost::random::uniform_int_distribution<>(_min, _max));
        }
        return uniform_int_generator;
    }

    auto&
    normal_real_generator(bool doDelete = false)
    {
        thread_local static boost::variate_generator<boost::lagged_fibonacci607&,
            boost::normal_distribution<>>* normal_real_generator;
        if (doDelete) {
            delete normal_real_generator;
            normal_real_generator = nullptr;
            return normal_real_generator;
        }
        if (normal_real_generator == nullptr) {
            normal_real_generator = new boost::variate_generator<boost::lagged_fibonacci607&,
                boost::normal_distribution<>>(
                random_engine(), boost::normal_distribution<>(0., 1.));
        }
        return normal_real_generator;
    }

public:
    RandomLaggedFibonacci607();
    ~RandomLaggedFibonacci607() override;

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
