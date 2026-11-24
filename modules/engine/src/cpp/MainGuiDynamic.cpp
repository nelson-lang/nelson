//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef NELSON_GUI_ENABLED
#include "MainGuiDynamic.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "GuiEngine.hpp"
#include <cstdio>
#include <cstdlib>
#include <stdexcept>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    struct GuiBackend
    {
        library_handle handle = nullptr;
        decltype(&NelsonGuiCreateEvaluator) create = nullptr;
        decltype(&NelsonGuiStartFigures) figures = nullptr;
        decltype(&NelsonGuiCreateNotebookKernelEvaluator) notebookKernel = nullptr;
        decltype(&NelsonGuiCreateDapEvaluator) dap = nullptr;
        decltype(&NelsonGuiWaitUntilLaunched) wait = nullptr;
        decltype(&NelsonGuiPumpEvents) pump = nullptr;
        decltype(&NelsonGuiShutdown) shutdown = nullptr;
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
    GuiBackend
    loadBackend()
    {
        GuiBackend backend;
        const auto filename = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory()
            + L"/libnlsGui" + get_dynamic_library_extensionW();
        backend.handle = load_dynamic_libraryW(filename);
        if (backend.handle == nullptr) {
            throw std::runtime_error("Cannot load Web GUI backend: " + get_dynamic_library_error());
        }
        try {
            backend.create
                = resolve<decltype(backend.create)>(backend.handle, "NelsonGuiCreateEvaluator");
            backend.figures
                = resolve<decltype(backend.figures)>(backend.handle, "NelsonGuiStartFigures");
            backend.notebookKernel = resolve<decltype(backend.notebookKernel)>(
                backend.handle, "NelsonGuiCreateNotebookKernelEvaluator");
            backend.dap
                = resolve<decltype(backend.dap)>(backend.handle, "NelsonGuiCreateDapEvaluator");
            backend.wait
                = resolve<decltype(backend.wait)>(backend.handle, "NelsonGuiWaitUntilLaunched");
            backend.pump = resolve<decltype(backend.pump)>(backend.handle, "NelsonGuiPumpEvents");
            backend.shutdown
                = resolve<decltype(backend.shutdown)>(backend.handle, "NelsonGuiShutdown");
        } catch (...) {
            close_dynamic_library(backend.handle);
            throw;
        }
        return backend;
    }
    //=============================================================================
    const GuiBackend&
    webGuiBackend()
    {
        // Interfaces and event callbacks keep referencing backend code until process exit.
        static const GuiBackend backend = []() {
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
CreateGuiEvaluatorDynamic(void* context, size_t id, const WebUiLaunchOptions& options)
{
    return webGuiBackend().create(context, id, options);
}
//=============================================================================
void
StartGuiFiguresDynamic(void* evaluator)
{
    webGuiBackend().figures(evaluator);
}
//=============================================================================
void*
CreateNotebookKernelEvaluatorDynamic(void* context, size_t id)
{
    return webGuiBackend().notebookKernel(context, id);
}
//=============================================================================
void*
CreateDapEvaluatorDynamic(void* context, size_t id)
{
    return webGuiBackend().dap(context, id);
}
//=============================================================================
bool
WaitGuiLaunchedDynamic(std::string& error)
{
    return webGuiBackend().wait(error);
}
//=============================================================================
void
ProcessGuiEventsDynamic()
{
    webGuiBackend().pump();
}
//=============================================================================
void
ShutdownGuiDynamic()
{
    webGuiBackend().shutdown();
}
//=============================================================================
} // namespace Nelson
#endif
//=============================================================================
