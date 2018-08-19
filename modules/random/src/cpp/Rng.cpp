//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Rng.hpp"
#include "Error.hpp"
#include "RandomInterface.hpp"
#include "RandomLaggedFibonacci607.hpp"
#include "RandomMersenneTwister.hpp"
#include "RandomMersenneTwister64.hpp"
#include "Rng_helpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
RngGetType(Evaluator* eval)
{
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
    return randEngine->getGeneratorName();
}
//=============================================================================
void
RngSetSeed(Evaluator* eval, double seed)
{
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        RandomMersenneTwister* randEngine = (RandomMersenneTwister*)eval->RandomEngine;
        uint32 s = (uint32)seed;
        randEngine->setSeed(s);
    } break;
    case RNG_TWISTER64: {
        RandomMersenneTwister64* randEngine = (RandomMersenneTwister64*)eval->RandomEngine;
        uint64 s = (uint64)seed;
        randEngine->setSeed(s);
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        RandomLaggedFibonacci607* randEngine = (RandomLaggedFibonacci607*)eval->RandomEngine;
        uint32 s = (uint32)seed;
        randEngine->setSeed(s);
    } break;
    }
}
//=============================================================================
ArrayOf
RngGetSeed(Evaluator* eval)
{
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
    ArrayOf res;
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        RandomMersenneTwister* randEngine = (RandomMersenneTwister*)eval->RandomEngine;
        res = ArrayOf::uint32Constructor(randEngine->getSeed());
    } break;
    case RNG_TWISTER64: {
        RandomMersenneTwister64* randEngine = (RandomMersenneTwister64*)eval->RandomEngine;
        res = ArrayOf::uint64Constructor(randEngine->getSeed());
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        RandomLaggedFibonacci607* randEngine = (RandomLaggedFibonacci607*)eval->RandomEngine;
        res = ArrayOf::uint32Constructor(randEngine->getSeed());
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
RngGetState(Evaluator* eval)
{
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    ArrayOf state;
    RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        RandomMersenneTwister* randEngine = (RandomMersenneTwister*)eval->RandomEngine;
        boost::container::vector<uint32> uint32State = randEngine->getState();
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
        RandomMersenneTwister64* randEngine = (RandomMersenneTwister64*)eval->RandomEngine;
        boost::container::vector<uint64> uint64State = randEngine->getState();
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
        RandomLaggedFibonacci607* randEngine = (RandomLaggedFibonacci607*)eval->RandomEngine;
        boost::container::vector<uint32> uint32State = randEngine->getState();
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
    }
    return state;
}
//=============================================================================
void
RngSetDefault(Evaluator* eval)
{
    if (eval->RandomEngine) {
        RngDelete(eval);
    }
    if (eval->RandomEngine == nullptr) {
        eval->RandomEngine = (void*)new RandomMersenneTwister();
    }
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
}
//=============================================================================
void
RngShuffle(Evaluator* eval)
{
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    switch (rngtype) {
    case RNG_TWISTER: {
        uint32 newseed = static_cast<uint32>(std::time(0));
        RandomMersenneTwister* randEngine = (RandomMersenneTwister*)eval->RandomEngine;
        randEngine->setSeed(newseed);
    } break;
    case RNG_TWISTER64: {
        RandomMersenneTwister64* randEngine = (RandomMersenneTwister64*)eval->RandomEngine;
        uint64 newseed = static_cast<uint64>(std::time(0));
        randEngine->setSeed(newseed);
    } break;
    case RNG_LAGGED_FIBONACCI_607: {
        uint32 newseed = static_cast<uint32>(std::time(0));
        RandomLaggedFibonacci607* randEngine = (RandomLaggedFibonacci607*)eval->RandomEngine;
        randEngine->setSeed(newseed);
    } break;
    }
}
//=============================================================================
bool
RngSetEngine(Evaluator* eval, double seed, std::wstring engineName)
{
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    if (isRngType(engineName)) {
        RngDelete(eval);
        RNG_TYPE newrngtype = getRngType(engineName);
        switch (newrngtype) {
        case RNG_TWISTER: {
            eval->RandomEngine = (void*)new RandomMersenneTwister();
        } break;
        case RNG_TWISTER64: {
            eval->RandomEngine = (void*)new RandomMersenneTwister64();
        } break;
        case RNG_LAGGED_FIBONACCI_607: {
            eval->RandomEngine = (void*)new RandomLaggedFibonacci607();
        } break;
        }
        if (eval->RandomEngine == nullptr) {
            Error(_W("random engine not initialized."));
        }
        RngSetSeed(eval, seed);
    } else {
        Error(_W("A valid generator expected."));
    }
    return false;
}
//=============================================================================
void
RngDelete(Evaluator* eval)
{
    if (eval->RandomEngine != nullptr) {
        RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
        delete randEngine;
        randEngine = nullptr;
        eval->RandomEngine = nullptr;
    }
}
//=============================================================================
bool
RngSetState(Evaluator* eval, ArrayOf st)
{
    RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
    RNG_TYPE rngtype = getRngType(randEngine->getGeneratorName());
    Class stClass = st.getDataClass();
    switch (rngtype) {
    case RNG_TWISTER: {
        if (stClass == NLS_UINT32) {
            if (st.isVector()) {
                if (st.getDimensions().getElementCount() == randEngine->getStateSize()) {
                    uint32* vec = (uint32*)st.getDataPointer();
                    RandomMersenneTwister* randEngine = (RandomMersenneTwister*)eval->RandomEngine;
                    randEngine->setState(vec, st.getDimensions().getElementCount());
                    return true;
                } else {
                    std::wstring msg = _W("dimensions of state must be") + L" "
                        + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                    Error(msg);
                }
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
                if (st.getDimensions().getElementCount() == randEngine->getStateSize()) {
                    uint64* vec = (uint64*)st.getDataPointer();
                    RandomMersenneTwister64* randEngine
                        = (RandomMersenneTwister64*)eval->RandomEngine;
                    randEngine->setState(vec, st.getDimensions().getElementCount());
                    return true;
                } else {
                    std::wstring msg = _W("dimensions of state must be") + L" "
                        + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                    Error(msg);
                }
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
                if (st.getDimensions().getElementCount() == randEngine->getStateSize()) {
                    uint32* vec = (uint32*)st.getDataPointer();
                    RandomLaggedFibonacci607* randEngine
                        = (RandomLaggedFibonacci607*)eval->RandomEngine;
                    randEngine->setState(vec, st.getDimensions().getElementCount());
                    return true;
                } else {
                    std::wstring msg = _W("dimensions of state must be") + L" "
                        + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                    Error(msg);
                }
            } else {
                std::wstring msg = _W("dimensions of state must be") + L" "
                    + std::to_wstring(randEngine->getStateSize()) + std::wstring(L"x1.");
                Error(msg);
            }
        } else {
            Error(_W("type of state must be uint32."));
        }
    } break;
    }
    return false;
}
};
//=============================================================================
