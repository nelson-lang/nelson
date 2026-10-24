//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef NELSON_WEB_GUI_ENABLED
#include "MainWebGuiDynamic.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "WebGuiEngine.hpp"
#include <cstdio>
#include <cstdlib>
#include <stdexcept>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    struct WebGuiBackend
    {
        library_handle handle = nullptr;
        decltype(&NelsonWebGuiCreateEvaluator) create = nullptr;
        decltype(&NelsonWebGuiStartFigures) figures = nullptr;
        decltype(&NelsonWebGuiWaitUntilLaunched) wait = nullptr;
        decltype(&NelsonWebGuiPumpEvents) pump = nullptr;
        decltype(&NelsonWebGuiShutdown) shutdown = nullptr;
    };
    //=============================================================================
    template <typename Function>
    Function
    resolve(library_handle handle, const char* name)
    {
        auto function = reinterpret_cast<Function>(get_function(handle, name));
        if (function == nullptr) {
            throw std::runtime_error(std::string("Missing Web GUI backend function: ") + name);
        }
        return function;
    }
    //=============================================================================
    WebGuiBackend
    loadBackend()
    {
        WebGuiBackend backend;
        const auto filename = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory()
            + L"/libnlsWeb_gui" + get_dynamic_library_extensionW();
        backend.handle = load_dynamic_libraryW(filename);
        if (backend.handle == nullptr) {
            throw std::runtime_error("Cannot load Web GUI backend: " + get_dynamic_library_error());
        }
        try {
            backend.create
                = resolve<decltype(backend.create)>(backend.handle, "NelsonWebGuiCreateEvaluator");
            backend.figures
                = resolve<decltype(backend.figures)>(backend.handle, "NelsonWebGuiStartFigures");
            backend.wait
                = resolve<decltype(backend.wait)>(backend.handle, "NelsonWebGuiWaitUntilLaunched");
            backend.pump
                = resolve<decltype(backend.pump)>(backend.handle, "NelsonWebGuiPumpEvents");
            backend.shutdown
                = resolve<decltype(backend.shutdown)>(backend.handle, "NelsonWebGuiShutdown");
        } catch (...) {
            close_dynamic_library(backend.handle);
            throw;
        }
        return backend;
    }
    //=============================================================================
    const WebGuiBackend&
    webGuiBackend()
    {
        // Interfaces and event callbacks keep referencing backend code until process exit.
        static const WebGuiBackend backend = []() {
            try {
                return loadBackend();
            } catch (const std::exception& error) {
                std::fprintf(stderr, "%s\n", error.what());
                std::exit(EXIT_FAILURE);
            }
        }();
        return backend;
    }
    //=============================================================================
} // namespace
//=============================================================================
void*
CreateWebGuiEvaluatorDynamic(void* context, size_t id, const WebUiLaunchOptions& options)
{
    return webGuiBackend().create(context, id, options);
}
//=============================================================================
void
StartWebGuiFiguresDynamic(void* evaluator)
{
    webGuiBackend().figures(evaluator);
}
//=============================================================================
bool
WaitWebGuiLaunchedDynamic(std::string& error)
{
    return webGuiBackend().wait(error);
}
//=============================================================================
void
ProcessWebGuiEventsDynamic()
{
    webGuiBackend().pump();
}
//=============================================================================
void
ShutdownWebGuiDynamic()
{
    webGuiBackend().shutdown();
}
//=============================================================================
} // namespace Nelson
#endif
//=============================================================================
