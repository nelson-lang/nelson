//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FmuComponent.hpp"
#ifdef __EMSCRIPTEN__
#include "WasmFmuComponent.hpp"
#endif
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    static FmuComponentFactory&
    factoryRef()
    {
        static FmuComponentFactory factory;
        return factory;
    }
    //=============================================================================
    void
    registerFmuComponentFactory(FmuComponentFactory factory)
    {
        factoryRef() = std::move(factory);
    }
    //=============================================================================
    bool
    hasFmuComponentFactory()
    {
        return static_cast<bool>(factoryRef());
    }
    //=============================================================================
    std::unique_ptr<IFmuComponent>
    makeFmuComponent(const std::string& fmuPath, SimDiagnostic& diag)
    {
        FmuComponentFactory& factory = factoryRef();
        if (!factory) {
#ifdef __EMSCRIPTEN__
            return makeWasmFmuComponent(fmuPath, diag);
#endif
            diag.code = "fmu_unavailable";
            diag.message = "FMI support is not available (the nflow_fmi module is not loaded).";
            diag.fatal = true;
            return nullptr;
        }
        return factory(fmuPath, diag);
    }
    //=============================================================================
    static FmuMeComponentFactory&
    meFactoryRef()
    {
        static FmuMeComponentFactory factory;
        return factory;
    }
    //=============================================================================
    void
    registerFmuMeComponentFactory(FmuMeComponentFactory factory)
    {
        meFactoryRef() = std::move(factory);
    }
    //=============================================================================
    bool
    hasFmuMeComponentFactory()
    {
        return static_cast<bool>(meFactoryRef());
    }
    //=============================================================================
    std::unique_ptr<IFmuMeComponent>
    makeFmuMeComponent(const std::string& fmuPath, SimDiagnostic& diag)
    {
        FmuMeComponentFactory& factory = meFactoryRef();
        if (!factory) {
#ifdef __EMSCRIPTEN__
            return makeWasmFmuMeComponent(fmuPath, diag);
#endif
            diag.code = "fmu_unavailable";
            diag.message = "FMI support is not available (the nflow_fmi module is not loaded).";
            diag.fatal = true;
            return nullptr;
        }
        return factory(fmuPath, diag);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
