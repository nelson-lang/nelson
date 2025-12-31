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
#ifndef M_PI
#define M_PI 3.141592653589793
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// Ziggurat method for normal distribution
// tables and constants based on Marsaglia and Tsang's paper
// Reference: "The Ziggurat Method for Generating Random Variables"
//=============================================================================
// Ziggurat algorithm constants
static const int N = 256;
static const double R = 3.6542;
//=============================================================================
// Pre-computed Ziggurat tables
static uint32_t kn[N];
static double wn[N], fn[N];
static bool ziggurat_initialized = false;
//=============================================================================
// Remove SHR3 - use only MRG32k3a for all random numbers
//=============================================================================
// Initialize Ziggurat tables
static void
zigset()
{
    if (ziggurat_initialized) {
        return;
    }

    // For n=256, zn = 3.6542 (from Cleve Moler's blog)
    double z[N + 1];
    z[N] = R; // 3.6542

    // Calculate the area under one section
    double area = z[N] * std::exp(-0.5 * z[N] * z[N]);
    // Add tail area: integral from zn to infinity of exp(-x^2/2) dx
    area += std::sqrt(2.0 * M_PI) * 0.5 * std::erfc(z[N] / std::sqrt(2.0));

    // Calculate z values from n down to 1
    for (int i = N - 1; i >= 1; i--) {
        z[i] = std::sqrt(-2.0 * std::log(area / z[i + 1] + std::exp(-0.5 * z[i + 1] * z[i + 1])));
    }
    z[0] = 0.0;

    // Set up lookup tables
    for (int i = 0; i < N; i++) {
        // kn[i] stores the boundary values
        if (i == 0) {
            kn[i] = (uint32_t)(z[1] / area * 4294967296.0);
        } else {
            kn[i] = (uint32_t)(z[i] / z[i + 1] * 4294967296.0);
        }

        // wn[i] stores the sigma values (core fractions)
        if (i == 0) {
            wn[i] = 0.0; // Top section has no core
        } else {
            wn[i] = z[i - 1] / z[i];
        }

        // fn[i] stores the z values
        fn[i] = z[i];
    }

    ziggurat_initialized = true;
}
//=============================================================================
static double
RNOR(RandomMRG32k3a* rng)
{
    if (!ziggurat_initialized) {
        zigset();
    }

    // actual implementation: randi(256) and 2*rand-1
    int j = (int)(rng->nextUniform() * N) + 1; // randi(256) - 1 to 256
    double u = 2.0 * rng->nextUniform() - 1.0; // 2*rand-1

    if (j < 1) {
        j = 1;
    }
    if (j > N) {
        j = N;
    }

    // Check if u falls in the core of the jth section
    // Most sigma values are > 0.98, this succeeds 98.5% of the time
    if (std::abs(u) < wn[j - 1]) { // Using wn as sigma approximation for now
        return u * fn[j - 1]; // Return u*z(j)
    }

    // Handle tip computation (< 1.5% of the time)
    if (j == 1) {
        // Top section has no core - handle infinite tail
        double x, y;
        do {
            x = -std::log(rng->nextUniform()) / R;
            y = -std::log(rng->nextUniform());
        } while (y + y < x * x);
        return (u > 0) ? R + x : -(R + x);
    }

    // Point falls in tip region - rejection method
    double x = u * fn[j - 1]; // u*z(j)
    if (kn[j - 1] + rng->nextUniform() * (kn[j - 2] - kn[j - 1]) < std::exp(-0.5 * x * x)) {
        return x;
    }

    // Rejection - try again
    return RNOR(rng);
}
//=============================================================================
RandomMRG32k3a::RandomMRG32k3a()
{
    seed = 12345;
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
    if (_seed == 0) {
        // Default seed values when seed is 0 (as used by MATLAB)
        s10 = 2637242579.0; // x1
        s11 = 1747669293.0; // x2
        s12 = 2112977067.0; // x3
        s20 = 2633436652.0; // y1
        s21 = 3518603782.0; // y2
        s22 = 229738972.0; // y3
    } else {
        s10 = s11 = s12 = static_cast<double>(_seed);
        s20 = s21 = s22 = static_cast<double>(_seed);
    }
}
//=============================================================================
uint32_t
RandomMRG32k3a::getSeed()
{
    return seed;
}
//=============================================================================
double
RandomMRG32k3a::nextUniform()
{
    long k;
    double p1, p2;

    // MRG32k3a constants (from L'Ecuyer's original implementation)
    static const double norm = 2.328306549295728e-10;
    static const double m1 = 4294967087.0;
    static const double m2 = 4294944443.0;
    static const double a12 = 1403580.0;
    static const double a13n = 810728.0;
    static const double a21 = 527612.0;
    static const double a23n = 1370589.0;

    // combRecursive uses every other value from MRG32k3a
    // Generate two values but return only the first one
    double result;

    for (int i = 0; i < 2; i++) {
        /* Component 1 */
        p1 = a12 * s11 - a13n * s10;
        k = static_cast<long>(p1 / m1);
        p1 -= k * m1;
        if (p1 < 0.0) {
            p1 += m1;
        }
        s10 = s11;
        s11 = s12;
        s12 = p1;

        /* Component 2 */
        p2 = a21 * s22 - a23n * s20;
        k = static_cast<long>(p2 / m2);
        p2 -= k * m2;
        if (p2 < 0.0) {
            p2 += m2;
        }
        s20 = s21;
        s21 = s22;
        s22 = p2;

        /* Combination */
        if (i == 0) { // Store the first value
            if (p1 <= p2) {
                result = ((p1 - p2 + m1) * norm);
            } else {
                result = ((p1 - p2) * norm);
            }
        }
        // Skip the second value (i == 1)
    }

    return result;
}
//=============================================================================
int
RandomMRG32k3a::nextInt(int min, int max)
{
    double u = nextUniform();
    return min + static_cast<int>(u * (max - min + 1));
}
//=============================================================================
double
RandomMRG32k3a::nextNormal()
{
    // Use Nearest-compatible Ziggurat method
    return RNOR(this);
}
//=============================================================================
void
RandomMRG32k3a::getValuesAsDouble(
    double* ar, indexType nbElements, indexType lastDim, RNG_DISTRIBUTION_TYPE _type)
{
    if (lastDim == 0 || lastDim == 1 || nbElements <= lastDim) {
        // Linear case - fill directly
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
        // Matrix case - fill in column-major order
        size_t numRows = nbElements / lastDim;
        size_t numCols = lastDim;

        // Fill column by column
        for (size_t col = 0; col < numCols; ++col) {
            for (size_t row = 0; row < numRows; ++row) {
                size_t index = col * numRows + row; // Column-major index
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
        // Linear case - fill directly
        for (indexType k = 0; k < nbElements; ++k) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[k] = static_cast<float>(nextUniform());
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[k] = static_cast<float>(nextInt(minInt, maxInt));
                break;
            case RNG_DISTRIBUTION_NORMAL:
                ar[k] = static_cast<float>(nextNormal());
                break;
            default:
                ar[k] = static_cast<float>(std::nan(""));
                break;
            }
        }
    } else {
        // Matrix case - fill in column-major order
        size_t numRows = nbElements / lastDim;
        size_t numCols = lastDim;

        // Fill column by column
        for (size_t col = 0; col < numCols; ++col) {
            for (size_t row = 0; row < numRows; ++row) {
                size_t index = col * numRows + row; // Column-major index
                switch (_type) {
                case RNG_DISTRIBUTION_UNIFORM_REAL:
                    ar[index] = static_cast<float>(nextUniform());
                    break;
                case RNG_DISTRIBUTION_UNIFORM_INT:
                    ar[index] = static_cast<float>(nextInt(minInt, maxInt));
                    break;
                case RNG_DISTRIBUTION_NORMAL:
                    ar[index] = static_cast<float>(nextNormal());
                    break;
                default:
                    ar[index] = static_cast<float>(std::nan(""));
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
RandomMRG32k3a::setMinMaxUniformIntDistribution(int min, int max)
{
    minInt = min;
    maxInt = max;
}
//=============================================================================
void
RandomMRG32k3a::getMinMaxUniformIntDistribution(int& min, int& max)
{
    min = minInt;
    max = maxInt;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
