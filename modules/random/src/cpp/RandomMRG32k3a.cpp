//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RandomMRG32k3a.hpp"
#include <cmath>
#include <cstdint>
//=============================================================================
namespace Nelson {
//=============================================================================
// MRG32k3a (combined multiple recursive generator, L'Ecuyer 1999).
//=============================================================================
static const double m1 = 4294967087.0;
static const double m2 = 4294944443.0;
static const double a12 = 1403580.0;
static const double a13n = 810728.0;
static const double a21 = 527612.0;
static const double a23n = 1370589.0;
static const double norm = 2.328306549295728e-10;
// Low-order term used to widen a pair of MRG outputs to a 53-bit double.
static const double norm2 = norm * (1.0 / 16777216.0); // norm * 2^-24
//=============================================================================
// Affine expansion of a scalar seed into the six-word state:
// word i = (kSeedOffset[i] + seed * kSeedStride[i]) mod kStateModulus[i].
// First three words live modulo m1, last three modulo m2.
static const uint64_t kSeedOffset[6]
    = { 0x9D3120D3ULL, 0x682B512DULL, 0x7DF178ABULL, 0x9CF70DECULL, 0xD1B9A206ULL, 0x0DB189DCULL };
static const uint64_t kSeedStride[6]
    = { 0x15CDB297ULL, 0xD3FB9028ULL, 0x2F7ADEABULL, 0x2EB32853ULL, 0x7FC1FF52ULL, 0xACAC8601ULL };
static const uint64_t kStateModulus[6]
    = { 0xFFFFFF2FULL, 0xFFFFFF2FULL, 0xFFFFFF2FULL, 0xFFFFA6BBULL, 0xFFFFA6BBULL, 0xFFFFA6BBULL };
//=============================================================================
RandomMRG32k3a::RandomMRG32k3a()
{
    seed = 0;
    setSeed(seed);
}
//=============================================================================
RandomMRG32k3a::~RandomMRG32k3a() { }
//=============================================================================
std::wstring
RandomMRG32k3a::getGeneratorName()
{
    return std::wstring(L"combRecursive");
}
//=============================================================================
void
RandomMRG32k3a::setSeed(uint32_t _seed)
{
    seed = _seed;
    has_gauss = false;
    cached_gauss = 0.0;
    uint64_t s[6];
    for (int i = 0; i < 6; ++i) {
        uint64_t prod = (static_cast<uint64_t>(_seed) * kSeedStride[i]) % kStateModulus[i];
        s[i] = (kSeedOffset[i] + prod) % kStateModulus[i];
    }
    s10 = static_cast<double>(s[0]);
    s11 = static_cast<double>(s[1]);
    s12 = static_cast<double>(s[2]);
    s20 = static_cast<double>(s[3]);
    s21 = static_cast<double>(s[4]);
    s22 = static_cast<double>(s[5]);
}
//=============================================================================
uint32_t
RandomMRG32k3a::getSeed()
{
    return seed;
}
//=============================================================================
double
RandomMRG32k3a::mrgStep()
{
    long k;
    double p1, p2;

    p1 = a12 * s11 - a13n * s10;
    k = static_cast<long>(p1 / m1);
    p1 -= k * m1;
    if (p1 < 0.0) {
        p1 += m1;
    }
    s10 = s11;
    s11 = s12;
    s12 = p1;

    p2 = a21 * s22 - a23n * s20;
    k = static_cast<long>(p2 / m2);
    p2 -= k * m2;
    if (p2 < 0.0) {
        p2 += m2;
    }
    s20 = s21;
    s21 = s22;
    s22 = p2;

    return (p1 <= p2) ? (p1 - p2 + m1) : (p1 - p2);
}
//=============================================================================
double
RandomMRG32k3a::nextUniform()
{
    // Two successive MRG outputs are combined into a single 53-bit double.
    double z1 = mrgStep();
    double z2 = mrgStep();
    return z1 * norm + z2 * norm2;
}
//=============================================================================
int64
RandomMRG32k3a::nextInt(int64 min, int64 max)
{
    double u = nextUniform();
    return min + static_cast<int64>(u * static_cast<double>(max - min + 1));
}
//=============================================================================
double
RandomMRG32k3a::nextNormal()
{
    // Polar (Marsaglia) method, matching the twister engine so that randn is
    // consistent across generators. The spare value is cached per instance.
    if (has_gauss) {
        has_gauss = false;
        return cached_gauss;
    }
    double x1, x2, r2;
    do {
        x1 = 2.0 * nextUniform() - 1.0;
        x2 = 2.0 * nextUniform() - 1.0;
        r2 = x1 * x1 + x2 * x2;
    } while (r2 >= 1.0 || r2 == 0.0);
    double f = std::sqrt(-2.0 * std::log(r2) / r2);
    cached_gauss = f * x1;
    has_gauss = true;
    return f * x2;
}
//=============================================================================
void
RandomMRG32k3a::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    if (lastDim == 0 || lastDim == 1 || nbElements <= lastDim) {
        for (indexType k = 0; k < nbElements; ++k) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[k] = nextUniform();
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[k] = static_cast<double>(nextInt(minInt, maxInt));
                break;
            case RNG_DISTRIBUTION_NORMAL:
                ar[k] = nextNormal();
                break;
            default:
                ar[k] = std::nan("");
                break;
            }
        }
    } else {
        size_t numRows = nbElements / lastDim;
        size_t numCols = lastDim;
        for (size_t col = 0; col < numCols; ++col) {
            for (size_t row = 0; row < numRows; ++row) {
                size_t index = col * numRows + row;
                switch (_type) {
                case RNG_DISTRIBUTION_UNIFORM_REAL:
                    ar[index] = nextUniform();
                    break;
                case RNG_DISTRIBUTION_UNIFORM_INT:
                    ar[index] = static_cast<double>(nextInt(minInt, maxInt));
                    break;
                case RNG_DISTRIBUTION_NORMAL:
                    ar[index] = nextNormal();
                    break;
                default:
                    ar[index] = std::nan("");
                    break;
                }
            }
        }
    }
}
//=============================================================================
void
RandomMRG32k3a::getValuesAsSingle(
    single* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    if (lastDim == 0 || lastDim == 1 || nbElements <= lastDim) {
        for (indexType k = 0; k < nbElements; ++k) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[k] = static_cast<single>(nextUniform());
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[k] = static_cast<single>(nextInt(minInt, maxInt));
                break;
            case RNG_DISTRIBUTION_NORMAL:
                ar[k] = static_cast<single>(nextNormal());
                break;
            default:
                ar[k] = static_cast<single>(std::nan(""));
                break;
            }
        }
    } else {
        size_t numRows = nbElements / lastDim;
        size_t numCols = lastDim;
        for (size_t col = 0; col < numCols; ++col) {
            for (size_t row = 0; row < numRows; ++row) {
                size_t index = col * numRows + row;
                switch (_type) {
                case RNG_DISTRIBUTION_UNIFORM_REAL:
                    ar[index] = static_cast<single>(nextUniform());
                    break;
                case RNG_DISTRIBUTION_UNIFORM_INT:
                    ar[index] = static_cast<single>(nextInt(minInt, maxInt));
                    break;
                case RNG_DISTRIBUTION_NORMAL:
                    ar[index] = static_cast<single>(nextNormal());
                    break;
                default:
                    ar[index] = static_cast<single>(std::nan(""));
                    break;
                }
            }
        }
    }
}
//=============================================================================
std::vector<uint32_t>
RandomMRG32k3a::getState()
{
    return { static_cast<uint32_t>(s10), static_cast<uint32_t>(s11), static_cast<uint32_t>(s12),
        static_cast<uint32_t>(s20), static_cast<uint32_t>(s21), static_cast<uint32_t>(s22),
        static_cast<uint32_t>(s10), static_cast<uint32_t>(s11), static_cast<uint32_t>(s12),
        static_cast<uint32_t>(s20), static_cast<uint32_t>(s21), static_cast<uint32_t>(s22) };
}
//=============================================================================
void
RandomMRG32k3a::setState(const uint32* _state, size_t len)
{
    if (len >= 6) {
        s10 = static_cast<double>(_state[0]);
        s11 = static_cast<double>(_state[1]);
        s12 = static_cast<double>(_state[2]);
        s20 = static_cast<double>(_state[3]);
        s21 = static_cast<double>(_state[4]);
        s22 = static_cast<double>(_state[5]);
        has_gauss = false;
    }
}
//=============================================================================
size_t
RandomMRG32k3a::getStateSize()
{
    return 12;
}
//=============================================================================
void
RandomMRG32k3a::setMinMaxUniformIntDistribution(int64 min, int64 max)
{
    minInt = min;
    maxInt = max;
}
//=============================================================================
void
RandomMRG32k3a::getMinMaxUniformIntDistribution(int64& min, int64& max)
{
    min = minInt;
    max = maxInt;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
