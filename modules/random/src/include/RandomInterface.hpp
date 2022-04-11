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
#include "Types.hpp"
#include "nlsRandom_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
typedef enum
{
    RNG_DISTRIBUTION_ERROR = -1,
    RNG_DISTRIBUTION_UNIFORM_REAL = 0,
    RNG_DISTRIBUTION_UNIFORM_INT,
    RNG_DISTRIBUTION_NORMAL
} RNG_DISTRIBUTION_TYPE;

class NLSRANDOM_IMPEXP RandomInterface
{
public:
    RandomInterface();
    virtual ~RandomInterface();

    virtual std::wstring
    getGeneratorName()
        = 0;
    virtual double
    getValueAsDouble(RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL)
        = 0;
    virtual single
    getValueAsSingle(RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL)
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
};
} // namespace Nelson
//=============================================================================
