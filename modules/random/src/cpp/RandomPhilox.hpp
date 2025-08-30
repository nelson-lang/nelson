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
#include "RandomInterface.hpp"
#include <string>
#include <vector>
#include <cstdint>
#include <random>
#include <philox4x32_10.h>
//=============================================================================
namespace Nelson {
//=============================================================================
#ifndef M_PI
#define M_PI 3.141592653589793
#endif
//=============================================================================
class RandomPhilox : public RandomInterface
{
public:
    //=============================================================================
    RandomPhilox(uint32_t seed = 0)
    {
        setSeed(seed);
        setMinMaxUniformIntDistribution(0, 0xFFFFFFFF);
    }
    //=============================================================================
    virtual ~RandomPhilox() { }
    //=============================================================================
    std::wstring
    getGeneratorName() override
    {
        return L"philox";
    }
    //=============================================================================
    uint32_t
    getSeed() override
    {
        return seed_value;
    }
    //=============================================================================
    std::vector<uint32_t>
    getState() const
    {
        std::vector<uint32_t> state(7);
        state[0] = counter[0];
        state[1] = counter[1];
        state[2] = counter[2];
        state[3] = counter[3];
        state[4] = key[0];
        state[5] = key[1];
        state[6] = 0;
        return state;
    }
    //=============================================================================
    void
    setState(const uint32_t* _state, size_t len) override
    {
        if (len >= 7) {
            counter[0] = _state[0];
            counter[1] = _state[1];
            counter[2] = _state[2];
            counter[3] = _state[3];
            key[0] = _state[4];
            key[1] = _state[5];
            position = 0;
            buffer_valid = false;
        }
    }
    //=============================================================================
    void
    setSeed(uint32_t seed)
    {
        seed_value = seed;
        counter[0] = 0;
        counter[1] = 0;
        counter[2] = seed;
        counter[3] = seed + 1;
        key[0] = seed + 2;
        key[1] = seed + 3;
        position = 0;
        buffer_valid = false;
    }
    //=============================================================================
    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        indexType i = 0;
        while (i < nbElements) {
            generateBuffer();
            counter[2] += 2;
            for (int j = 0; j < 4 && i < nbElements; ++j, ++i) {
                uint32_t nextUint32 = buffer[j];
                if (_type == RNG_DISTRIBUTION_UNIFORM_REAL) {
                    ar[i] = static_cast<double>(nextUint32) / 4294967296.0;
                } else if (_type == RNG_DISTRIBUTION_NORMAL) {
                    if (i + 1 < nbElements) {
                        uint32_t u1_int = nextUint32;
                        uint32_t u2_int = (j + 1 < 4) ? buffer[j + 1] : getNextUint32();
                        double u1 = (static_cast<double>(u1_int) + 0.5) / 4294967296.0;
                        double u2 = (static_cast<double>(u2_int) + 0.5) / 4294967296.0;
                        double val1 = std::sqrt(-2.0 * std::log(u1)) * std::cos(2.0 * M_PI * u2);
                        double val2 = std::sqrt(-2.0 * std::log(u1)) * std::sin(2.0 * M_PI * u2);
                        ar[i] = val1;
                        ++i;
                        if (i < nbElements) {
                            ar[i] = val2;
                        }
                        ++j;
                    } else {
                        double u1 = (static_cast<double>(nextUint32) + 0.5) / 4294967296.0;
                        ar[i] = std::sqrt(-2.0 * std::log(u1)) * std::cos(2.0 * M_PI * 0.5);
                    }
                } else if (_type == RNG_DISTRIBUTION_UNIFORM_INT) {
                    ar[i] = static_cast<double>(_min + (nextUint32 % (_max - _min + 1)));
                }
            }
        }
    }
    //=============================================================================
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        indexType i = 0;
        while (i < nbElements) {
            generateBuffer();
            counter[2] += 2;
            for (int j = 0; j < 4 && i < nbElements; ++j, ++i) {
                uint32_t nextUint32 = buffer[j];
                if (_type == RNG_DISTRIBUTION_UNIFORM_REAL) {
                    ar[i] = static_cast<float>(nextUint32) / 4294967296.0f;
                } else if (_type == RNG_DISTRIBUTION_NORMAL) {
                    if (i + 1 < nbElements) {
                        uint32_t u1_int = nextUint32;
                        uint32_t u2_int = (j + 1 < 4) ? buffer[j + 1] : getNextUint32();
                        float u1 = (static_cast<float>(u1_int) + 0.5f) / 4294967296.0f;
                        float u2 = (static_cast<float>(u2_int) + 0.5f) / 4294967296.0f;
                        ar[i] = static_cast<single>(
                            std::sqrt(-2.0f * std::log(u1)) * std::cos(2.0f * M_PI * u2));
                        ++i;
                        if (i < nbElements) {
                            ar[i] = static_cast<single>(
                                std::sqrt(-2.0f * std::log(u1)) * std::sin(2.0f * M_PI * u2));
                        }
                        ++j;
                    } else {
                        float u1 = (static_cast<float>(nextUint32) + 0.5f) / 4294967296.0f;
                        ar[i] = static_cast<single>(
                            std::sqrt(-2.0f * std::log(u1)) * std::cos(2.0f * M_PI * 0.5f));
                    }
                } else if (_type == RNG_DISTRIBUTION_UNIFORM_INT) {
                    ar[i] = static_cast<single>(_min + (nextUint32 % (_max - _min + 1)));
                }
            }
        }
    }
    //=============================================================================
    void
    setMinMaxUniformIntDistribution(int min, int max) override
    {
        _min = min;
        _max = max;
    }
    //=============================================================================
    void
    getMinMaxUniformIntDistribution(int& min, int& max) override
    {
        min = _min;
        max = _max;
    }
    //=============================================================================
    size_t
    getStateSize() override
    {
        return 7;
    }
    //=============================================================================
private:
    uint32_t counter[4] = { 0, 0, 0, 1 };
    uint32_t key[2] = { 2, 3 };
    uint32_t position = 0;
    uint32_t buffer[4] = { 0 };
    bool buffer_valid = false;
    uint32_t seed_value = 0;
    int _min = 0;
    int _max = 0xFFFFFFFF;
    //=============================================================================
    uint32_t
    getNextUint32()
    {
        generateBuffer();
        return buffer[0];
    }
    //=============================================================================
    void
    generateBuffer()
    {
        buffer[0] = counter[0];
        buffer[1] = counter[1];
        buffer[2] = counter[2];
        buffer[3] = counter[3];
        uint32_t working_key[2] = { key[0], key[1] };
        philox4x32_10_alt(buffer, working_key);
        buffer_valid = true;
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
