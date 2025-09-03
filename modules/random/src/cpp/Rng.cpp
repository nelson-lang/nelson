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
#include "RandomPhilox.hpp"
#include "dSFMT19937.hpp"
#include "RandomMRG32k3a.hpp"
#include "RandomThreefry.hpp"
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

    auto setSeedForEngine = [&](auto seedValue) { randEngine->setSeed(seedValue); };

    switch (rngtype) {
    case RNG_TWISTER:
    case RNG_DSFMT19937:
    case RNG_MRG32K3A:
    case RNG_LAGGED_FIBONACCI_607:
    case RNG_PHILOX:
    case RNG_THREEFRY: {
        randEngine->setSeed(static_cast<uint32>(seed));
    } break;
    case RNG_TWISTER64: {
        auto randEngineTwister64 = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        randEngineTwister64->setSeed(static_cast<uint64>(seed));
    } break;
    default: {
        Error(_W("random engine not managed."));
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

    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());

    switch (rngtype) {
    case RNG_TWISTER64: {
        auto* engine = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        return ArrayOf::uint64Constructor(engine->getSeedU64());
    }
    case RNG_TWISTER:
    case RNG_DSFMT19937:
    case RNG_MRG32K3A:
    case RNG_LAGGED_FIBONACCI_607:
    case RNG_PHILOX:
    case RNG_THREEFRY: {
        return ArrayOf::uint32Constructor(randEngine->getSeed());
    }
    default:
        return ArrayOf();
    }
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

    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());

    auto buildArrayOf = [](auto&& vec, NelsonType type) -> ArrayOf {
        using T = typename std::decay<decltype(vec[0])>::type;
        T* mat = (T*)ArrayOf::allocateArrayOf(type, vec.size(), Nelson::stringVector(), false);
        for (size_t k = 0; k < vec.size(); k++) {
            mat[k] = vec[k];
        }
        Dimensions dims;
        dims[0] = vec.size();
        dims[1] = 1;
        return ArrayOf(type, dims, mat, false);
    };

    switch (rngtype) {
    case RNG_TWISTER: {
        auto* engine = static_cast<RandomMersenneTwister*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        return buildArrayOf(engine->getState(), NLS_UINT32);
    }
    case RNG_TWISTER64: {
        auto* engine = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        return buildArrayOf(engine->getState(), NLS_UINT64);
    }
    case RNG_DSFMT19937: {
        auto* engine
            = static_cast<dSFMT19937*>(NelsonConfiguration::getInstance()->getRandomEngine());
        return buildArrayOf(engine->getState(), NLS_UINT32);
    }
    case RNG_MRG32K3A: {
        auto* engine
            = static_cast<RandomMRG32k3a*>(NelsonConfiguration::getInstance()->getRandomEngine());
        return buildArrayOf(engine->getState(), NLS_UINT32);
    }
    case RNG_LAGGED_FIBONACCI_607: {
        auto* engine = static_cast<RandomLaggedFibonacci607*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        return buildArrayOf(engine->getState(), NLS_UINT32);
    }
    case RNG_PHILOX: {
        auto* engine
            = static_cast<RandomPhilox*>(NelsonConfiguration::getInstance()->getRandomEngine());
        return buildArrayOf(engine->getState(), NLS_UINT32);
    }
    case RNG_THREEFRY: {
        auto* engine
            = static_cast<RandomThreefry*>(NelsonConfiguration::getInstance()->getRandomEngine());
        return buildArrayOf(engine->getState(), NLS_UINT32);
    }
    default:
        return ArrayOf();
    }
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

    auto setSeedForEngine = [&](auto* engine, auto seed) { engine->setSeed(seed); };

    auto now = time(nullptr);

    switch (rngtype) {
    case RNG_TWISTER: {
        setSeedForEngine(static_cast<RandomMersenneTwister*>(
                             NelsonConfiguration::getInstance()->getRandomEngine()),
            static_cast<uint32>(now));
        break;
    }
    case RNG_TWISTER64: {
        setSeedForEngine(static_cast<RandomMersenneTwister64*>(
                             NelsonConfiguration::getInstance()->getRandomEngine()),
            static_cast<uint64>(now));
        break;
    }
    case RNG_DSFMT19937: {
        setSeedForEngine(
            static_cast<dSFMT19937*>(NelsonConfiguration::getInstance()->getRandomEngine()),
            static_cast<uint32>(now));
        break;
    }
    case RNG_MRG32K3A: {
        setSeedForEngine(
            static_cast<RandomMRG32k3a*>(NelsonConfiguration::getInstance()->getRandomEngine()),
            static_cast<uint32>(now));
        break;
    }
    case RNG_LAGGED_FIBONACCI_607: {
        setSeedForEngine(static_cast<RandomLaggedFibonacci607*>(
                             NelsonConfiguration::getInstance()->getRandomEngine()),
            static_cast<uint32>(now));
        break;
    }
    case RNG_PHILOX: {
        setSeedForEngine(
            static_cast<RandomPhilox*>(NelsonConfiguration::getInstance()->getRandomEngine()),
            static_cast<uint32>(now));
        break;
    }
    case RNG_THREEFRY: {
        setSeedForEngine(
            static_cast<RandomThreefry*>(NelsonConfiguration::getInstance()->getRandomEngine()),
            static_cast<uint32>(now));
        break;
    }
    default:
        break;
    }
}
//=============================================================================
bool
RngSetEngine(double seed, const std::wstring& engineName)
{
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        Error(_W("random engine not initialized."));
    }
    if (!isRngType(engineName)) {
        Error(_W("A valid generator expected."));
    }
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
    case RNG_DSFMT19937: {
        dSFMT19937* tmp = new dSFMT19937();
        NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        RandomLaggedFibonacci607* tmp = new RandomLaggedFibonacci607();
        NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
    } break;
    case RNG_MRG32K3A: {
        RandomMRG32k3a* tmp = new RandomMRG32k3a();
        NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
    } break;
    case RNG_PHILOX: {
        RandomPhilox* tmp = new RandomPhilox();
        NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
    } break;
    case RNG_THREEFRY: {
        RandomThreefry* tmp = new RandomThreefry();
        NelsonConfiguration::getInstance()->setRandomEngine((void*)tmp);
    } break;
    default: {
    } break;
    }
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        Error(_W("random engine not initialized."));
    }
    RngSetSeed(seed);
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

    auto isValidState = [&](NelsonType expectedType) -> bool {
        if (stClass != expectedType) {
            Error(_W("type of state must be ")
                + (expectedType == NLS_UINT32 ? L"uint32." : L"uint64."));
            return false;
        }
        if (!st.isVector() || st.getElementCount() != randEngine->getStateSize()) {
            std::wstring msg = _W("dimensions of state must be") + L" "
                + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
            Error(msg);
            return false;
        }
        return true;
    };

    switch (rngtype) {
    case RNG_TWISTER:
    case RNG_DSFMT19937:
    case RNG_LAGGED_FIBONACCI_607:
    case RNG_MRG32K3A:
    case RNG_PHILOX:
    case RNG_THREEFRY: {
        if (isValidState(NLS_UINT32)) {
            auto* vec = (uint32*)st.getDataPointer();
            randEngine->setState(vec, st.getElementCount());
            return true;
        }
        break;
    }
    case RNG_TWISTER64: {
        if (isValidState(NLS_UINT64)) {
            auto* vec = (uint64*)st.getDataPointer();
            auto* randEngineTwister64 = static_cast<RandomMersenneTwister64*>(
                NelsonConfiguration::getInstance()->getRandomEngine());
            randEngineTwister64->setState(vec, st.getElementCount());
            return true;
        }
        break;
    }
    default:
        break;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
