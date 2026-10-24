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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "RandomInterface.hpp"
#include "RandomLaggedFibonacci607.hpp"
#include "RandomMersenneTwister.hpp"
#include "RandomMersenneTwister64.hpp"
#include "RandomPhilox.hpp"
#include "dSFMT19937.hpp"
#include "RandomMRG32k3a.hpp"
#include "RandomThreefry.hpp"
#include "RandomPcg64Dxsm.hpp"
#include "RandomXoshiro256pp.hpp"
#include "Rng_helpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static RandomInterface*
createEngine(RNG_TYPE rngType);
static void
installEngine(const ArrayOf& settings, bool seedOnlyWhenStateEmpty);
//=============================================================================
std::wstring
RngGetType()
{
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
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
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
    }
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());

    auto setSeedForEngine = [&](auto seedValue) { randEngine->setSeed(seedValue); };

    switch (rngtype) {
    case RNG_TWISTER:
    case RNG_DSFMT19937:
    case RNG_MRG32K3A:
    case RNG_LAGGED_FIBONACCI_607:
    case RNG_PHILOX:
    case RNG_THREEFRY:
    case RNG_PCG64DXSM:
    case RNG_XOSHIRO256PP: {
        randEngine->setSeed(static_cast<uint32>(seed));
    } break;
    case RNG_TWISTER64: {
        auto randEngineTwister64 = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        randEngineTwister64->setSeed(static_cast<uint64>(seed));
    } break;
    default: {
        Error(L"Nelson:rng:engineNotManaged", L"random engine not supported.");
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
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
    }

    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());

    switch (rngtype) {
    case RNG_TWISTER64: {
        auto* engine = static_cast<RandomMersenneTwister64*>(
            NelsonConfiguration::getInstance()->getRandomEngine());
        return ArrayOf::uint64Scalar(engine->getSeedU64());
    }
    case RNG_TWISTER:
    case RNG_DSFMT19937:
    case RNG_MRG32K3A:
    case RNG_LAGGED_FIBONACCI_607:
    case RNG_PHILOX:
    case RNG_THREEFRY:
    case RNG_PCG64DXSM:
    case RNG_XOSHIRO256PP: {
        return ArrayOf::uint32Scalar(randEngine->getSeed());
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
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
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
    case RNG_PCG64DXSM:
    case RNG_XOSHIRO256PP: {
        auto* engine = static_cast<RandomUint64StateInterface*>(randEngine);
        return buildArrayOf(engine->getState64(), NLS_UINT64);
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
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
    }
}
//=============================================================================
void
RngShuffle()
{
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
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
    case RNG_PCG64DXSM:
    case RNG_XOSHIRO256PP: {
        randEngine->setSeed(static_cast<uint32>(now));
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
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
    }
    if (!isRngType(engineName)) {
        Error(L"Nelson:rng:invalidGeneratorName", L"A valid generator name expected.");
    }
    RngDelete();
    NelsonConfiguration::getInstance()->setRandomEngine(createEngine(getRngType(engineName)));
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
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
    NelsonType stClass = st.dataClass();

    auto isValidState = [&](NelsonType expectedType) -> bool {
        if (stClass != expectedType) {
            Error(L"Nelson:rng:invalidStateType", L"Type of state must be '%s'.",
                (expectedType == NLS_UINT32 ? L"uint32" : L"uint64"));
            return false;
        }
        if (!st.isVector() || st.elementCount() != randEngine->getStateSize()) {
            std::wstring msg = formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:random:dimensionsStateDx1", L"Dimensions of state must be %dx1."),
                (int)randEngine->getStateSize());
            Error(L"Nelson:rng:invalidState", msg);
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
            auto* vec = (uint32*)st.dataPointer();
            randEngine->setState(vec, st.elementCount());
            return true;
        }
        break;
    }
    case RNG_TWISTER64: {
        if (isValidState(NLS_UINT64)) {
            auto* vec = (uint64*)st.dataPointer();
            auto* randEngineTwister64 = static_cast<RandomMersenneTwister64*>(
                NelsonConfiguration::getInstance()->getRandomEngine());
            randEngineTwister64->setState(vec, st.elementCount());
            return true;
        }
        break;
    }
    case RNG_PCG64DXSM:
    case RNG_XOSHIRO256PP: {
        if (isValidState(NLS_UINT64)) {
            auto* engine = static_cast<RandomUint64StateInterface*>(randEngine);
            if (!engine->setState64((uint64*)st.dataPointer(), st.elementCount())) {
                Error(L"Nelson:rng:invalidSettings",
                    L"Must contain generator settings captured previously.");
            }
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
ArrayOf
RngGetSettings()
{
    stringVector fieldnames = { "Type", "Seed", "State" };
    ArrayOfVector fieldvalues;
    fieldvalues.push_back(ArrayOf::characterArray(RngGetType()));
    fieldvalues.push_back(RngGetSeed());
    fieldvalues.push_back(RngGetState());
    return ArrayOf::structArray(fieldnames, fieldvalues);
}
//=============================================================================
void
RngSetSettings(const ArrayOf& settings)
{
    installEngine(settings, false);
}
//=============================================================================
RngEngineScope::RngEngineScope(const ArrayOf& settings)
    : previousEngine(NelsonConfiguration::getInstance()->getRandomEngine())
{
    NelsonConfiguration::getInstance()->setRandomEngine(nullptr);
    try {
        installEngine(settings, true);
    } catch (...) {
        RngDelete();
        NelsonConfiguration::getInstance()->setRandomEngine(previousEngine);
        throw;
    }
}
//=============================================================================
RngEngineScope::~RngEngineScope()
{
    RngDelete();
    NelsonConfiguration::getInstance()->setRandomEngine(previousEngine);
}
//=============================================================================
static RandomInterface*
createEngine(RNG_TYPE rngType)
{
    switch (rngType) {
    case RNG_TWISTER:
        return new RandomMersenneTwister();
    case RNG_TWISTER64:
        return new RandomMersenneTwister64();
    case RNG_DSFMT19937:
        return new dSFMT19937();
    case RNG_LAGGED_FIBONACCI_607:
        return new RandomLaggedFibonacci607();
    case RNG_MRG32K3A:
        return new RandomMRG32k3a();
    case RNG_PHILOX:
        return new RandomPhilox();
    case RNG_THREEFRY:
        return new RandomThreefry();
    case RNG_PCG64DXSM:
        return new RandomPcg64Dxsm();
    case RNG_XOSHIRO256PP:
        return new RandomXoshiro256pp();
    default:
        return nullptr;
    }
}
//=============================================================================
static void
installEngine(const ArrayOf& settings, bool seedOnlyWhenStateEmpty)
{
    stringVector fieldnames = settings.isStruct() ? settings.fieldNames() : stringVector();
    if (!settings.isScalar() || fieldnames.size() != 3 || fieldnames[0] != "Type"
        || fieldnames[1] != "Seed" || fieldnames[2] != "State") {
        Error(
            L"Nelson:rng:invalidSettings", L"Must contain generator settings captured previously.");
    }
    std::wstring engineName = settings.field("Type").asWideString();
    double seed = settings.field("Seed").asDoubleScalar();
    if (!isRngType(engineName)) {
        Error(L"Nelson:rng:invalidGeneratorName", L"A valid generator name expected.");
    }
    RngDelete();
    NelsonConfiguration::getInstance()->setRandomEngine(createEngine(getRngType(engineName)));
    RngSetSeed(seed);
    ArrayOf state = settings.field("State");
    if (!(seedOnlyWhenStateEmpty && state.isEmpty())) {
        RngSetState(state);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
