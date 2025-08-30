//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <randomkit.h>
#include <distributions.h>
#include "RandomMersenneTwister.hpp"
//=============================================================================
#define MAGIC_SEED 5489
//=============================================================================
namespace Nelson {
//=============================================================================
RandomMersenneTwister::RandomMersenneTwister() : pImpl(new Impl())
{
    uint32 initial_seed = 0;
    rk_seed((initial_seed == 0 ? MAGIC_SEED : initial_seed), &pImpl->state);
    seed = initial_seed;
}
//=============================================================================
RandomMersenneTwister::~RandomMersenneTwister() { delete pImpl; }
//=============================================================================
std::wstring
RandomMersenneTwister::getGeneratorName()
{
    return std::wstring(L"twister");
}
//=============================================================================
void
RandomMersenneTwister::setSeed(uint32 _seed)
{
    std::lock_guard<std::mutex> lock(mtx);
    seed = _seed;
    rk_seed((seed == 0 ? MAGIC_SEED : seed), &pImpl->state);
}
//=============================================================================
uint32
RandomMersenneTwister::getSeed()
{
    std::lock_guard<std::mutex> lock(mtx);
    return seed;
}
//=============================================================================
void
RandomMersenneTwister::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    int local_min, local_max;
    rk_state local_state;

    {
        std::lock_guard<std::mutex> lock(mtx);
        local_min = pImpl->int_min;
        local_max = pImpl->int_max;
        local_state = pImpl->state;
    }

    size_t p = (nbElements / lastDim);
    for (size_t k = 0; k < p; k++) {
        for (indexType l = 0; l < lastDim; l++) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[k * lastDim + l] = rk_double(&local_state);
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[k * lastDim + l] = static_cast<double>(local_min
                    + static_cast<int>(rk_double(&local_state) * (local_max - local_min + 1)));
                break;
            case RNG_DISTRIBUTION_NORMAL:
                ar[k * lastDim + l] = rk_normal(&local_state, 0.0, 1.0);
                break;
            default:
                break;
            }
        }
    }

    {
        std::lock_guard<std::mutex> lock(mtx);
        pImpl->state = local_state;
    }
}
//=============================================================================
void
RandomMersenneTwister::getValuesAsSingle(
    single* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    int local_min, local_max;
    rk_state local_state;

    {
        std::lock_guard<std::mutex> lock(mtx);
        local_min = pImpl->int_min;
        local_max = pImpl->int_max;
        local_state = pImpl->state;
    }

    size_t p = (nbElements / lastDim);
    for (size_t k = 0; k < p; k++) {
        for (indexType l = 0; l < lastDim; l++) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[k * lastDim + l] = static_cast<single>(rk_double(&local_state));
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[k * lastDim + l] = static_cast<single>(local_min
                    + static_cast<int>(rk_double(&local_state) * (local_max - local_min + 1)));
                break;
            case RNG_DISTRIBUTION_NORMAL:
                ar[k * lastDim + l] = static_cast<single>(rk_normal(&local_state, 0.0, 1.0));
                break;
            default:
                break;
            }
        }
    }

    {
        std::lock_guard<std::mutex> lock(mtx);
        pImpl->state = local_state;
    }
}
//=============================================================================
std::vector<uint32>
RandomMersenneTwister::getState()
{
    std::lock_guard<std::mutex> lock(mtx);
    std::vector<uint32> state(RK_STATE_LEN + 1);
    std::memcpy(state.data(), pImpl->state.key, RK_STATE_LEN * sizeof(uint32));
    state[RK_STATE_LEN] = pImpl->state.pos;
    return state;
}
//=============================================================================
void
RandomMersenneTwister::setState(const uint32* _state, size_t len)
{
    std::lock_guard<std::mutex> lock(mtx);
    if (len == RK_STATE_LEN + 1) {
        std::memcpy(pImpl->state.key, _state, RK_STATE_LEN * sizeof(uint32));
        pImpl->state.pos = _state[RK_STATE_LEN];
    }
}
//=============================================================================
size_t
RandomMersenneTwister::getStateSize()
{
    return RK_STATE_LEN + 1;
}
//=============================================================================
void
RandomMersenneTwister::setMinMaxUniformIntDistribution(int _min, int _max)
{
    std::lock_guard<std::mutex> lock(mtx);
    pImpl->int_min = _min;
    pImpl->int_max = _max;
}
//=============================================================================
void
RandomMersenneTwister::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    std::lock_guard<std::mutex> lock(mtx);
    _min = pImpl->int_min;
    _max = pImpl->int_max;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
