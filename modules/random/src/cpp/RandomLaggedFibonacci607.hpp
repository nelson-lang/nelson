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
#include <functional>
#include <cmath>
#include "RandomInterface.hpp"
#include "LaggedFibonacci607Engine.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class RandomLaggedFibonacci607 : public RandomInterface
{
private:
    void* generator = nullptr;
    uint32 seed = 0;
    bool hasSpare_ = false;
    double spare_ = 0.0;
    int minInt = 1;
    int maxInt = 1000;

    // Use the custom Lagged Fibonacci 607 engine
    using EngineType = LaggedFibonacci607Engine;
    using UniformRealDist = std::uniform_real_distribution<double>;

    auto&
    random_engine()
    {
        thread_local static EngineType rngLaggedFibonacci607;
        return rngLaggedFibonacci607;
    }

    double
    uniform01()
    {
        thread_local static UniformRealDist dist(0.0, 1.0);
        return dist(random_engine());
    }

    auto&
    uniform_real_generator(bool doDelete = false)
    {
        thread_local static UniformRealDist dist(0.0, 1.0);
        thread_local static auto* gen
            = new std::function<double()>([this]() mutable { return dist(random_engine()); });
        if (doDelete) {
            delete gen;
            gen = nullptr;
            return gen;
        }
        if (!gen) {
            gen = new std::function<double()>([this]() mutable { return dist(random_engine()); });
        }
        return gen;
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

    auto&
    normal_real_generator(bool doDelete = false)
    {
        thread_local static auto* gen = new std::function<double()>([this]() { return normal(); });
        if (doDelete) {
            delete gen;
            gen = nullptr;
            return gen;
        }
        if (!gen) {
            gen = new std::function<double()>([this]() { return normal(); });
        }
        return gen;
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
    setMinMaxUniformIntDistribution(int _min, int _max) override;
    void
    getMinMaxUniformIntDistribution(int& _min, int& _max) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
