//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ArrayOf.hpp"
#include <algorithm>
#include <chrono>
#include <cstdio>
#include <stdexcept>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
makeValue(NelsonType type, int mode, const ArrayOf& matrix)
{
    if (mode == 3) {
        return ArrayOf(matrix);
    }
    if (type == NLS_DOUBLE) {
        if (mode == 0) {
            return ArrayOf::doubleScalar(3.5);
        }
        if (mode == 1) {
            return ArrayOf(type, Dimensions(1, 1), new double[1] { 3.5 });
        }
        return ArrayOf::doubleMatrix2d(1, 1);
    }
    if (type == NLS_SINGLE) {
        if (mode == 0) {
            return ArrayOf::singleScalar(3.5f);
        }
        if (mode == 1) {
            return ArrayOf(type, Dimensions(1, 1), new single[1] { 3.5f });
        }
        return ArrayOf::singleMatrix2d(1, 1);
    }
    if (mode == 0) {
        return ArrayOf::logicalScalar(true);
    }
    return ArrayOf(type, Dimensions(1, 1), new logical[1] { 1 });
}
//=============================================================================
static double
firstValue(const ArrayOf& value, NelsonType type)
{
    if (type == NLS_DOUBLE) {
        return *static_cast<const double*>(value.dataPointer());
    }
    if (type == NLS_SINGLE) {
        return *static_cast<const single*>(value.dataPointer());
    }
    return *static_cast<const logical*>(value.dataPointer());
}
//=============================================================================
int
main()
{
    try {
        constexpr int iterations = 2000000;
        for (NelsonType type : { NLS_DOUBLE, NLS_SINGLE, NLS_LOGICAL }) {
            ArrayOf matrix(type, Dimensions(2, 2), ArrayOf::allocateArrayOf(type, 4, {}, true));
            if (type == NLS_DOUBLE) {
                *static_cast<double*>(matrix.readWriteDataPointer()) = 3.5;
            } else if (type == NLS_SINGLE) {
                *static_cast<single*>(matrix.readWriteDataPointer()) = 3.5f;
            } else {
                *static_cast<logical*>(matrix.readWriteDataPointer()) = 1;
            }
            for (int mode = 0; mode < 4; ++mode) {
                if (type == NLS_LOGICAL && mode == 2) {
                    continue;
                }
                std::vector<double> times;
                for (int trial = 0; trial < 9; ++trial) {
                    double checksum = 0;
                    auto begin = std::chrono::steady_clock::now();
                    for (int k = 0; k < iterations; ++k) {
                        ArrayOf value = makeValue(type, mode, matrix);
                        checksum += firstValue(value, type);
                    }
                    double elapsed
                        = std::chrono::duration<double>(std::chrono::steady_clock::now() - begin)
                              .count();
                    const double expected = mode == 2 ? 0. : (type == NLS_LOGICAL ? 1. : 3.5);
                    if (checksum != iterations * expected) {
                        throw std::runtime_error("Invalid benchmark result");
                    }
                    times.push_back(elapsed);
                }
                std::sort(times.begin(), times.end());
                std::printf("NATIVE %d %d %.9f\n", int(type), mode, times[times.size() / 2]);
            }
        }
        std::printf("SIZE %zu\n", sizeof(ArrayOf));
        return 0;
    } catch (const std::exception& e) {
        std::fprintf(stderr, "%s\n", e.what());
        return 1;
    }
}
//=============================================================================
