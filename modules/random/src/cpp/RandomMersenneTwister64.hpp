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
#include "RandomInterface.hpp"
#include "MersenneTwister64Engine.hpp"
#include "ZigguratNormal.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class RandomMersenneTwister64 : public RandomInterface
{
private:
    uint64 seed = 0;
    int64 minInt = 1;
    int64 maxInt = 1000;

    // generator and distribution belong to the instance: two engines never
    // share a state
    MersenneTwister64Engine mersenneTwister;
    std::uniform_real_distribution<double> uniformDistribution { 0.0, 1.0 };

    MersenneTwister64Engine&
    random_engine()
    {
        return mersenneTwister;
    }

    double
    uniform01()
    {
        return uniformDistribution(mersenneTwister);
    }

    // same ziggurat as 'twister': one 64-bit word stands for its two 32-bit
    // words, and nothing is cached outside the engine state
    double
    normal()
    {
        return zigguratNormal(
            [this](uint32_t& w0, uint32_t& w1) {
                const uint64_t w = mersenneTwister();
                w0 = static_cast<uint32_t>(w >> 32);
                w1 = static_cast<uint32_t>(w);
            },
            [this]() { return uniform01(); });
    }

public:
    RandomMersenneTwister64();
    ~RandomMersenneTwister64() override;

    std::wstring
    getGeneratorName() override;

    void
    setSeed(uint32 _seed) override;
    void
    setSeed(uint64 _seed);
    uint32
    getSeed() override;
    uint64
    getSeedU64();

    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;

    std::vector<uint64>
    getState();

    void
    setState(const uint32* _state, size_t len) override;
    void
    setState(const uint64* _state, size_t len);
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
