//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#define _USE_MATH_DEFINES
#include <math.h>
#include "dSFMT19937.hpp"
#include <cstring>
//=============================================================================
namespace Nelson {
//=============================================================================
static const int N = 256;
static const double R = 3.6542;
static uint32_t kn[N];
static double wn[N], fn[N];
static bool ziggurat_initialized = false;
//=============================================================================
static void
zigset()
{
    if (ziggurat_initialized)
        return;

    double z[N + 1];
    z[N] = R;

    double area = z[N] * std::exp(-0.5 * z[N] * z[N]);
    area += std::sqrt(2.0 * M_PI) * 0.5 * std::erfc(z[N] / std::sqrt(2.0));

    for (int i = N - 1; i >= 1; i--) {
        z[i] = std::sqrt(-2.0 * std::log(area / z[i + 1] + std::exp(-0.5 * z[i + 1] * z[i + 1])));
    }
    z[0] = 0.0;

    for (int i = 0; i < N; i++) {
        if (i == 0) {
            kn[i] = (uint32_t)(z[1] / area * 4294967296.0);
        } else {
            kn[i] = (uint32_t)(z[i] / z[i + 1] * 4294967296.0);
        }
        wn[i] = (i == 0) ? 0.0 : z[i - 1] / z[i];
        fn[i] = z[i];
    }

    ziggurat_initialized = true;
}
//=============================================================================
static double
RNOR_dSFMT(dsfmt_t* state)
{
    if (!ziggurat_initialized)
        zigset();

    int j = (int)(dsfmt_genrand_close_open(state) * N) + 1;
    double u = 2.0 * dsfmt_genrand_close_open(state) - 1.0;

    if (j < 1)
        j = 1;
    if (j > N)
        j = N;

    if (std::abs(u) < wn[j - 1]) {
        return u * fn[j - 1];
    }

    if (j == 1) {
        double x, y;
        do {
            x = -std::log(dsfmt_genrand_close_open(state)) / R;
            y = -std::log(dsfmt_genrand_close_open(state));
        } while (y + y < x * x);
        return (u > 0) ? R + x : -(R + x);
    }

    double x = u * fn[j - 1];
    if (kn[j - 1] + dsfmt_genrand_close_open(state) * (kn[j - 2] - kn[j - 1])
        < std::exp(-0.5 * x * x)) {
        return x;
    }

    return RNOR_dSFMT(state);
}
//=============================================================================
dSFMT19937::dSFMT19937() : pImpl(new Impl())
{
    seed = 0;
    setSeed(seed);
    zigset();
}
//=============================================================================
dSFMT19937::~dSFMT19937() { delete pImpl; }
//=============================================================================
std::wstring
dSFMT19937::getGeneratorName()
{
    return std::wstring(L"simdTwister");
}
//=============================================================================
void
dSFMT19937::setSeed(uint32 _seed)
{
    seed = _seed;
    dsfmt_init_gen_rand(&pImpl->state, seed);
}
//=============================================================================
uint32
dSFMT19937::getSeed()
{
    return seed;
}
//=============================================================================
void
dSFMT19937::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    size_t p = (nbElements / lastDim);
    for (size_t k = 0; k < p; k++) {
        for (indexType l = 0; l < lastDim; l++) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[k * lastDim + l] = dsfmt_genrand_close_open(&pImpl->state);
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[k * lastDim + l] = static_cast<double>(pImpl->int_min
                    + static_cast<int>(dsfmt_genrand_close_open(&pImpl->state)
                        * (pImpl->int_max - pImpl->int_min + 1)));
                break;
            case RNG_DISTRIBUTION_NORMAL: {
                ar[k * lastDim + l] = RNOR_dSFMT(&pImpl->state);
                break;
            }
            default:
                break;
            }
        }
    }
}
//=============================================================================
void
dSFMT19937::getValuesAsSingle(
    single* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    size_t p = (nbElements / lastDim);
    for (size_t k = 0; k < p; k++) {
        for (indexType l = 0; l < lastDim; l++) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[k * lastDim + l] = static_cast<single>(dsfmt_genrand_close_open(&pImpl->state));
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[k * lastDim + l] = static_cast<single>(pImpl->int_min
                    + static_cast<int>(dsfmt_genrand_close_open(&pImpl->state)
                        * (pImpl->int_max - pImpl->int_min + 1)));
                break;
            case RNG_DISTRIBUTION_NORMAL: {
                ar[k * lastDim + l] = static_cast<single>(RNOR_dSFMT(&pImpl->state));
                break;
            }
            default:
                break;
            }
        }
    }
}
//=============================================================================
std::vector<uint32>
dSFMT19937::getState()
{
    std::vector<uint32> state(sizeof(pImpl->state) / sizeof(uint32), 0);
    std::memcpy(state.data(), &pImpl->state, sizeof(pImpl->state));
    return state;
}
//=============================================================================
void
dSFMT19937::setState(const uint32* _state, size_t len)
{
    if (len == sizeof(pImpl->state) / sizeof(uint32)) {
        std::memcpy(&pImpl->state, _state, sizeof(pImpl->state));
    }
}
//=============================================================================
size_t
dSFMT19937::getStateSize()
{
    return sizeof(pImpl->state) / sizeof(uint32);
}
//=============================================================================
void
dSFMT19937::setMinMaxUniformIntDistribution(int _min, int _max)
{
    pImpl->int_min = _min;
    pImpl->int_max = _max;
}
//=============================================================================
void
dSFMT19937::getMinMaxUniformIntDistribution(int& _min, int& _max)
{
    _min = pImpl->int_min;
    _max = pImpl->int_max;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
