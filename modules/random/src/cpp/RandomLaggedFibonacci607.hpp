//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include <random>
#include <string>
#include <cmath>
#include "RandomInterface.hpp"
#include "LaggedFibonacci607Engine.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class RandomLaggedFibonacci607 : public RandomInterface
{
private:
    uint32 seed = 0;
    bool hasSpare_ = false;
    double spare_ = 0.0;
    int64 minInt = 1;
    int64 maxInt = 1000;

    // Use the custom Lagged Fibonacci 607 engine
    using EngineType = LaggedFibonacci607Engine;
    using UniformRealDist = std::uniform_real_distribution<double>;

    // generator and distribution belong to the instance: two engines never
    // share a state
    EngineType rngLaggedFibonacci607;
    UniformRealDist uniformDistribution { 0.0, 1.0 };

    EngineType&
    random_engine()
    {
        return rngLaggedFibonacci607;
    }

    double
    uniform01()
    {
        return uniformDistribution(rngLaggedFibonacci607);
    }

    double
    normal(double mean = 0.0, double stddev = 1.0)
    {
        if (hasSpare_) {
            hasSpare_ = false;
            return mean + stddev * spare_;
        }

        double u, v, s;
        do {
            u = 2.0 * uniform01() - 1.0;
            v = 2.0 * uniform01() - 1.0;
            s = u * u + v * v;
        } while (s >= 1.0 || s == 0.0);

        double mul = std::sqrt(-2.0 * std::log(s) / s);
        spare_ = v * mul;
        hasSpare_ = true;
        return mean + stddev * (u * mul);
    }

public:
    RandomLaggedFibonacci607();
    ~RandomLaggedFibonacci607() override;

    std::wstring
    getGeneratorName() override;

    void
    setSeed(uint32 _seed) override;
    uint32
    getSeed() override;

    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;

    std::vector<uint32>
    getState();
    void
    setState(const uint32* _state, size_t len) override;
    size_t
    getStateSize() override;

    void
    setMinMaxUniformIntDistribution(int64 _min, int64 _max) override;
    void
    getMinMaxUniformIntDistribution(int64& _min, int64& _max) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
