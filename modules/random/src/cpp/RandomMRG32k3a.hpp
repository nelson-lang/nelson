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
#include <cmath>
#include <string>
#include <stdint.h>
#include <vector>
#include "RandomInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class RandomMRG32k3a : public RandomInterface
{
public:
    RandomMRG32k3a();
    ~RandomMRG32k3a() override;

    std::wstring
    getGeneratorName() override;

    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;

    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override;

    void
    setMinMaxUniformIntDistribution(int min, int max) override;

    void
    getMinMaxUniformIntDistribution(int& min, int& max) override;

    size_t
    getStateSize() override;

    void
    setSeed(uint32_t _seed) override;
    uint32_t
    getSeed() override;
    double
    nextUniform();
    int
    nextInt(int min, int max);
    double
    nextNormal();
    std::vector<uint32_t>
    getState();
    void
    setState(const uint32* _state, size_t len) override;

private:
    double s10, s11, s12; // First component state
    double s20, s21, s22; // Second component state
    uint32_t seed;
    int minInt, maxInt;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
