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
#include <string>
#include <mutex>
#include <randomkit.h>
#include "RandomInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class RandomMersenneTwister : public RandomInterface
{
private:
    struct Impl
    {
        rk_state state;
        int int_min = 1;
        int int_max = 1000;
    };

    Impl* pImpl;
    uint32 seed = 0;
    mutable std::mutex mtx;

public:
    RandomMersenneTwister();
    ~RandomMersenneTwister() override;

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
