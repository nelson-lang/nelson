//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
