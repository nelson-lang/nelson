//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Rng.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "RandomInterface.hpp"
#include "RandomLaggedFibonacci607.hpp"
#include "RandomMersenneTwister.hpp"
#include "RandomMersenneTwister64.hpp"
#include "Rng_helpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
RngGetType()
{
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    return randEngine->getGeneratorName();
}
//=============================================================================
void
RngSetSeed(double seed)
{
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        auto* randEngine = static_cast<RandomMersenneTwister*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        auto s = static_cast<uint32>(seed);
        randEngine->setSeed(s);
    } break;
    case RNG_TWISTER64: {
        auto* randEngine = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        auto s = static_cast<uint64>(seed);
        randEngine->setSeed(s);
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        auto* randEngine = static_cast<RandomLaggedFibonacci607*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        auto s = static_cast<uint32>(seed);
        randEngine->setSeed(s);
    } break;
    default: {
    } break;
    }
}
//=============================================================================
ArrayOf
RngGetSeed()
{
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    ArrayOf res;
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        auto* randEngine = static_cast<RandomMersenneTwister*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        res = ArrayOf::uint32Constructor(randEngine->getSeed());
    } break;
    case RNG_TWISTER64: {
        auto* randEngine = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        res = ArrayOf::uint64Constructor(randEngine->getSeed());
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        auto* randEngine = static_cast<RandomLaggedFibonacci607*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        res = ArrayOf::uint32Constructor(randEngine->getSeed());
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
RngGetState()
{
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    ArrayOf state;
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        auto* randEngine = static_cast<RandomMersenneTwister*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        std::vector<uint32> uint32State = randEngine->getState();
        uint32* mat = (uint32*)ArrayOf::allocateArrayOf(
            NLS_UINT32, uint32State.size(), Nelson::stringVector(), false);
        for (size_t k = 0; k < uint32State.size(); k++) {
            mat[k] = uint32State[k];
        }
        Dimensions dims;
        dims[0] = uint32State.size();
        dims[1] = 1;
        state = ArrayOf(NLS_UINT32, dims, mat, false);
    } break;
    case RNG_TWISTER64: {
        auto* randEngine = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        std::vector<uint64> uint64State = randEngine->getState();
        uint64* mat = (uint64*)ArrayOf::allocateArrayOf(
            NLS_UINT64, uint64State.size(), Nelson::stringVector(), false);
        for (size_t k = 0; k < uint64State.size(); k++) {
            mat[k] = uint64State[k];
        }
        Dimensions dims;
        dims[0] = uint64State.size();
        dims[1] = 1;
        state = ArrayOf(NLS_UINT64, dims, mat, false);
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        auto* randEngine = static_cast<RandomLaggedFibonacci607*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        std::vector<uint32> uint32State = randEngine->getState();
        uint32* mat = (uint32*)ArrayOf::allocateArrayOf(
            NLS_UINT32, uint32State.size(), Nelson::stringVector(), false);
        for (size_t k = 0; k < uint32State.size(); k++) {
            mat[k] = uint32State[k];
        }
        Dimensions dims;
        dims[0] = uint32State.size();
        dims[1] = 1;
        state = ArrayOf(NLS_UINT32, dims, mat, false);
    } break;
    default: {
    } break;
    }
    return state;
}
//=============================================================================
void
RngSetDefault()
{
    if (NelsonConfiguration::getInstance()->getRandomEngine() != nullptr) {
        RngDelete();
    }
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        try {
            RandomMersenneTwister* tmp = new RandomMersenneTwister();
            NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
        } catch (std::bad_alloc&) {
            NelsonConfiguration::getInstance()->setRandomEngine(nullptr);
        }
    }
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        Error(_W("random engine not initialized."));
    }
}
//=============================================================================
void
RngShuffle()
{
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        Error(_W("random engine not initialized."));
    }
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        uint32 newseed = static_cast<uint32>(std::time(nullptr));
        auto* randEngine = static_cast<RandomMersenneTwister*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        randEngine->setSeed(newseed);
    } break;
    case RNG_TWISTER64: {
        auto* randEngine = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        uint64 newseed = static_cast<uint64>(std::time(nullptr));
        randEngine->setSeed(newseed);
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        uint32 newseed = static_cast<uint32>(std::time(nullptr));
        auto* randEngine = static_cast<RandomLaggedFibonacci607*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        randEngine->setSeed(newseed);
    } break;
    default: {
    } break;
    }
}
//=============================================================================
bool
RngSetEngine(double seed, const std::wstring& engineName)
{
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        Error(_W("random engine not initialized."));
    }
    if (isRngType(engineName)) {
        RngDelete();
        RNG_TYPE newrngtype = getRngType(engineName);
        switch (newrngtype) {
        case RNG_TWISTER: {
            RandomMersenneTwister* tmp = new RandomMersenneTwister();
            NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
        } break;
        case RNG_TWISTER64: {
            RandomMersenneTwister64* tmp = new RandomMersenneTwister64();
            NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
        } break;
        case RNG_LAGGED_FIBONACCI_607: {
            RandomLaggedFibonacci607* tmp = new RandomLaggedFibonacci607();
            NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
        } break;
        default: {
        } break;
        }
        if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
            Error(_W("random engine not initialized."));
        }
        RngSetSeed(seed);
    } else {
        Error(_W("A valid generator expected."));
    }
    return false;
}
//=============================================================================
void
RngDelete()
{
    if (NelsonConfiguration::getInstance()->getRandomEngine() != nullptr) {
        auto* randEngine
            = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
        delete randEngine;
        randEngine = nullptr;
        NelsonConfiguration::getInstance()->setRandomEngine(nullptr);
    }
}
//=============================================================================
bool
RngSetState(const ArrayOf& st)
{
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    NelsonType stClass = st.getDataClass();
    switch (rngtype) {
    case RNG_TWISTER: {
        if (stClass == NLS_UINT32) {
            if (st.isVector()) {
                if (st.getElementCount() == randEngine->getStateSize()) {
                    auto* vec = (uint32*)st.getDataPointer();
                    auto* randEngine = static_cast<RandomMersenneTwister*>(
                        NelsonConfiguration::getInstance()->getRandomEngine());
                    randEngine->setState(vec, st.getElementCount());
                    return true;
                }
                std::wstring msg = _W("dimensions of state must be") + L" "
                    + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                Error(msg);

            } else {
                std::wstring msg = _W("dimensions of state must be") + L" "
                    + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                Error(msg);
            }
        } else {
            Error(_W("type of state must be uint32."));
        }
    } break;
    case RNG_TWISTER64: {
        if (stClass == NLS_UINT64) {
            if (st.isVector()) {
                if (st.getElementCount() == randEngine->getStateSize()) {
                    auto* vec = (uint64*)st.getDataPointer();
                    auto* randEngine = static_cast<RandomMersenneTwister64*>(
                        NelsonConfiguration::getInstance()->getRandomEngine());
                    randEngine->setState(vec, st.getElementCount());
                    return true;
                }
                std::wstring msg = _W("dimensions of state must be") + L" "
                    + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                Error(msg);

            } else {
                std::wstring msg = _W("dimensions of state must be") + L" "
                    + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                Error(msg);
            }
        } else {
            Error(_W("type of state must be uint64."));
        }
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        if (stClass == NLS_UINT32) {
            if (st.isVector()) {
                if (st.getElementCount() == randEngine->getStateSize()) {
                    auto* vec = (uint32*)st.getDataPointer();
                    auto* randEngine = static_cast<RandomLaggedFibonacci607*>(
                        NelsonConfiguration::getInstance()->getRandomEngine());
                    randEngine->setState(vec, st.getElementCount());
                    return true;
                }
                std::wstring msg = _W("dimensions of state must be") + L" "
                    + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                Error(msg);

            } else {
                std::wstring msg = _W("dimensions of state must be") + L" "
                    + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                Error(msg);
            }
        } else {
            Error(_W("type of state must be uint32."));
        }
    } break;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
