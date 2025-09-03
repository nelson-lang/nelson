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
#include <string>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    RNG_DISTRIBUTION_ERROR = -1,
    RNG_DISTRIBUTION_UNIFORM_REAL = 0,
    RNG_DISTRIBUTION_UNIFORM_INT,
    RNG_DISTRIBUTION_NORMAL
} RNG_DISTRIBUTION_TYPE;
//=============================================================================
class RandomInterface
{
public:
    RandomInterface();
    virtual ~RandomInterface();

    virtual std::wstring
    getGeneratorName()
        = 0;

    virtual void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL)
        = 0;
    virtual void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL)
        = 0;

    virtual void
    setMinMaxUniformIntDistribution(int _min, int _max)
        = 0;
    virtual void
    getMinMaxUniformIntDistribution(int& _min, int& _max)
        = 0;

    virtual size_t
    getStateSize()
        = 0;

    virtual void
    setState(const uint32* _state, size_t len)
        = 0;

    virtual void
    setSeed(uint32 _seed)
        = 0;

    virtual uint32
    getSeed()
        = 0;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
