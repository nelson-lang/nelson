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
#include <cmath>
#include <cstdint>
#include "RandomInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Common interface of the engines whose state is exposed as a uint64 vector.
class RandomUint64StateInterface : public RandomInterface
{
public:
    virtual std::vector<uint64>
    getState64() = 0;
    // returns false when the words do not describe a valid engine state.
    virtual bool
    setState64(const uint64* state, size_t len)
        = 0;
};
//=============================================================================
// Shared driver of the 64-bit output engines (pcg64dxsm, xoshiro256pp).
// Engine provides: static constexpr size_t N (state words),
//   static uint64 next(uint64* w), static void seed(uint64* w, uint64 seed),
//   static bool normalize(uint64* w) (fix up / validate restored words).
// State vector layout: [N engine words, precision mode, cached half word].
// Full precision: one 64-bit output per double, 53 random bits in (0,1).
// Reduced precision: one 32-bit half output per double, twice fewer outputs.
// Normal deviates use the inversion method (inverse normal CDF of a uniform).
template <typename Engine> class Random64BitEngine : public RandomUint64StateInterface
{
public:
    //=============================================================================
    Random64BitEngine() { setSeed(0); }
    //=============================================================================
    uint32
    getSeed() override
    {
        return seedValue;
    }
    //=============================================================================
    void
    setSeed(uint32 seed) override
    {
        seedValue = seed;
        Engine::seed(words, static_cast<uint64>(seed));
        fullPrecision = true;
        cacheValid = false;
        cachedHalf = 0;
    }
    //=============================================================================
    size_t
    getStateSize() override
    {
        return Engine::N + 2;
    }
    //=============================================================================
    std::vector<uint64>
    getState64() override
    {
        std::vector<uint64> state(Engine::N + 2);
        for (size_t k = 0; k < Engine::N; ++k) {
            state[k] = words[k];
        }
        state[Engine::N] = fullPrecision ? 0 : 1;
        state[Engine::N + 1]
            = cacheValid ? ((static_cast<uint64>(1) << 32) | cachedHalf) : static_cast<uint64>(0);
        return state;
    }
    //=============================================================================
    bool
    setState64(const uint64* state, size_t len) override
    {
        if (len != Engine::N + 2 || state[Engine::N] > 1 || (state[Engine::N + 1] >> 33) != 0) {
            return false;
        }
        uint64 w[Engine::N];
        for (size_t k = 0; k < Engine::N; ++k) {
            w[k] = state[k];
        }
        if (!Engine::normalize(w)) {
            return false;
        }
        for (size_t k = 0; k < Engine::N; ++k) {
            words[k] = w[k];
        }
        fullPrecision = (state[Engine::N] == 0);
        cacheValid = ((state[Engine::N + 1] >> 32) != 0);
        cachedHalf = static_cast<uint32>(state[Engine::N + 1]);
        return true;
    }
    //=============================================================================
    void
    setState(const uint32* state, size_t len) override
    {
        // uint32 states are not used by these engines (see setState64).
    }
    //=============================================================================
    void
    setMinMaxUniformIntDistribution(int64 min, int64 max) override
    {
        intMin = min;
        intMax = max;
    }
    //=============================================================================
    void
    getMinMaxUniformIntDistribution(int64& min, int64& max) override
    {
        min = intMin;
        max = intMax;
    }
    //=============================================================================
    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        fillUniform(ar, nbElements);
        switch (_type) {
        case RNG_DISTRIBUTION_UNIFORM_INT: {
            const double range = static_cast<double>(intMax - intMin + 1);
            for (indexType k = 0; k < nbElements; ++k) {
                ar[k] = static_cast<double>(intMin + static_cast<int64>(ar[k] * range));
            }
        } break;
        case RNG_DISTRIBUTION_NORMAL: {
            for (indexType k = 0; k < nbElements; ++k) {
                ar[k] = normalInverse(ar[k]);
            }
        } break;
        default: {
        } break;
        }
    }
    //=============================================================================
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        if (_type == RNG_DISTRIBUTION_UNIFORM_REAL) {
            fillUniformSingle(ar, nbElements);
            return;
        }
        const indexType blockSize = 4096;
        double block[4096];
        for (indexType start = 0; start < nbElements; start += blockSize) {
            indexType n = (nbElements - start < blockSize) ? (nbElements - start) : blockSize;
            getValuesAsDouble(block, n, lastDim, _type);
            for (indexType k = 0; k < n; ++k) {
                ar[start + k] = static_cast<single>(block[k]);
            }
        }
    }
    //=============================================================================
protected:
    uint64 words[Engine::N];
    //=============================================================================
private:
    uint32 seedValue = 0;
    bool fullPrecision = true;
    bool cacheValid = false;
    uint32 cachedHalf = 0;
    int64 intMin = 0;
    int64 intMax = 0xFFFFFFFF;
    //=============================================================================
    // (k + 0.5) / 2^32: 32 random bits, strictly inside (0,1).
    static inline double
    halfToDouble(uint32 h)
    {
        return (static_cast<double>(h) + 0.5) * 2.3283064365386963e-10;
    }
    //=============================================================================
    void
    fillUniform(double* ar, indexType n)
    {
        uint64 w[Engine::N];
        for (size_t k = 0; k < Engine::N; ++k) {
            w[k] = words[k];
        }
        indexType i = 0;
        if (fullPrecision) {
            for (; i < n; ++i) {
                uint64 m;
                do {
                    m = Engine::next(w) >> 11;
                } while (m == 0);
                // m < 2^53: signed conversion is exact and faster than the unsigned one.
                ar[i] = static_cast<double>(static_cast<int64>(m)) * 1.1102230246251565e-16;
            }
        } else {
            if (cacheValid && n > 0) {
                ar[i++] = halfToDouble(cachedHalf);
                cacheValid = false;
            }
            for (; i + 1 < n; i += 2) {
                uint64 x = Engine::next(w);
                ar[i] = halfToDouble(static_cast<uint32>(x >> 32));
                ar[i + 1] = halfToDouble(static_cast<uint32>(x));
            }
            if (i < n) {
                uint64 x = Engine::next(w);
                ar[i] = halfToDouble(static_cast<uint32>(x >> 32));
                cachedHalf = static_cast<uint32>(x);
                cacheValid = true;
            }
        }
        for (size_t k = 0; k < Engine::N; ++k) {
            words[k] = w[k];
        }
    }
    //=============================================================================
    // 24 random bits k in [1, 2^24 - 1], value k / 2^24 exact in single.
    void
    fillUniformSingle(single* ar, indexType n)
    {
        uint64 w[Engine::N];
        for (size_t k = 0; k < Engine::N; ++k) {
            w[k] = words[k];
        }
        for (indexType i = 0; i < n; ++i) {
            uint32 m;
            do {
                if (fullPrecision) {
                    m = static_cast<uint32>(Engine::next(w) >> 40);
                } else {
                    uint32 h;
                    if (cacheValid) {
                        h = cachedHalf;
                        cacheValid = false;
                    } else {
                        uint64 x = Engine::next(w);
                        h = static_cast<uint32>(x >> 32);
                        cachedHalf = static_cast<uint32>(x);
                        cacheValid = true;
                    }
                    m = h >> 8;
                }
            } while (m == 0);
            ar[i] = static_cast<single>(m) * 5.9604644775390625e-08f;
        }
        for (size_t k = 0; k < Engine::N; ++k) {
            words[k] = w[k];
        }
    }
    //=============================================================================
    // Inverse of the standard normal CDF for p in (0,1): Wichura algorithm
    // AS 241 (PPND16), rational approximations accurate to about 1e-16.
    static double
    normalInverse(double p)
    {
        static const double a[8] = { 3.3871328727963666080e0, 1.3314166789178437745e+2,
            1.9715909503065514427e+3, 1.3731693765509461125e+4, 4.5921953931549871457e+4,
            6.7265770927008700853e+4, 3.3430575583588128105e+4, 2.5090809287301226727e+3 };
        static const double b[8] = { 1.0, 4.2313330701600911252e+1, 6.8718700749205790830e+2,
            5.3941960214247511077e+3, 2.1213794301586595867e+4, 3.9307895800092710610e+4,
            2.8729085735721942674e+4, 5.2264952788528545610e+3 };
        static const double c[8] = { 1.42343711074968357734e0, 4.63033784615654529590e0,
            5.76949722146069140550e0, 3.64784832476320460504e0, 1.27045825245236838258e0,
            2.41780725177450611770e-1, 2.27238449892691845833e-2, 7.74545014278341407640e-4 };
        static const double d[8] = { 1.0, 2.05319162663775882187e0, 1.67638483018380384940e0,
            6.89767334985100004550e-1, 1.48103976427480074590e-1, 1.51986665636164571966e-2,
            5.47593808499534494600e-4, 1.05075007164441684324e-9 };
        static const double e[8] = { 6.65790464350110377720e0, 5.46378491116411436990e0,
            1.78482653991729133580e0, 2.96560571828504891230e-1, 2.65321895265761230930e-2,
            1.24266094738807843860e-3, 2.71155556874348757815e-5, 2.01033439929228813265e-7 };
        static const double f[8] = { 1.0, 5.99832206555887937690e-1, 1.36929880922735805310e-1,
            1.48753612908506148525e-2, 7.86869131145613259100e-4, 1.84631831751005468180e-5,
            1.42151175831644588870e-7, 2.04426310338993978564e-15 };
        const double q = p - 0.5;
        if (std::fabs(q) <= 0.425) {
            const double r = 0.180625 - q * q;
            return q * polynomial(a, r) / polynomial(b, r);
        }
        double r = std::sqrt(-std::log(q < 0 ? p : 1.0 - p));
        double x;
        if (r <= 5.0) {
            r -= 1.6;
            x = polynomial(c, r) / polynomial(d, r);
        } else {
            r -= 5.0;
            x = polynomial(e, r) / polynomial(f, r);
        }
        return q < 0 ? -x : x;
    }
    //=============================================================================
    static inline double
    polynomial(const double* coefficients, double x)
    {
        return ((((((coefficients[7] * x + coefficients[6]) * x + coefficients[5]) * x
                      + coefficients[4])
                         * x
                     + coefficients[3])
                        * x
                    + coefficients[2])
                       * x
                   + coefficients[1])
            * x
            + coefficients[0];
    }
    //=============================================================================
};
//=============================================================================
// SplitMix64 step, used to expand a 32-bit seed into engine state words.
inline uint64
splitMix64Next(uint64& x)
{
    uint64 z = (x += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
