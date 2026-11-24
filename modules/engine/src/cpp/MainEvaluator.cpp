//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdio>
#include <cstdlib>
#include <memory>
#include "AdvancedTerminal.hpp"
#include "MainEvaluator.hpp"
#include "BasicTerminal.hpp"
#ifdef NELSON_GUI_ENABLED
#include "MainGuiDynamic.hpp"
#include "WebUiHelpers.hpp"
#endif
#include "BuiltInFunctionDefManager.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "Clear.hpp"
#include "ClearGlobal.hpp"
#include "ComputionalThreads.hpp"
#include "Localization.hpp"
#include "ModulesManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
#include "RuntimeDispatch.hpp"
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Evaluator*
createMainEvaluator(NELSON_ENGINE_MODE _mode, const std::wstring& lang, bool minimizeWindow,
    const std::wstring& webUrl, uint16_t webPort, bool haveWebUrl, bool haveWebPort,
    bool webServerMode)
{
    auto& config = *NelsonConfiguration::getInstance();
    std::unique_ptr<Evaluator> mainEvaluator;
    size_t mainEvaluatorID = 0;
    setDefaultMaxNumCompThreads();
    if (config.getMainEvaluator() == nullptr) {
        std::unique_ptr<Context> context;
        try {
            context = std::make_unique<Context>();
        } catch (const std::bad_alloc&) {
            context = nullptr;
        }
        std::wstring effectiveLang = Localization::Instance()->initializeLocalization(lang);
        config.setMainGuiObject(nullptr);

        if (context) {
            const std::string msg = Nelson::resolveMessageTemplate(
                "Nelson:engine:modeNotImplemented", "This mode is not yet implemented.\n");
            switch (_mode) {
            case BASIC_ENGINE:
                fprintf(stderr, "%s", msg.c_str());
                exit(1);
                break;
            case ADVANCED_ENGINE:
                fprintf(stderr, "%s", msg.c_str());
                exit(1);
                break;
            case BASIC_TERMINAL:
            case WASM_TERMINAL: {
                std::unique_ptr<BasicTerminal> nlsTerm;
                try {
                    nlsTerm = std::make_unique<BasicTerminal>();
                } catch (const std::bad_alloc&) {
                }
                if (nlsTerm) {
                    // The WebAssembly engine hosts figures, timers and their callbacks
                    // on its single thread; pause(), drawnow() and the idle pump
                    // service them. nelson-cli (BASIC_TERMINAL) has none of these.
                    const bool haveEventsLoop = (_mode == WASM_TERMINAL);
                    mainEvaluator = std::make_unique<Evaluator>(
                        context.release(), nlsTerm.release(), haveEventsLoop, mainEvaluatorID);
                }
                break;
            }
            case ADVANCED_TERMINAL: {
#ifdef NELSON_GUI_ENABLED
                // Notebook kernel: the interpreter is driven over a stdio JSON-RPC
                // frame channel, figures served headless on the same channel. No GUI
                // objects, so figure RPC dispatches in place while the kernel is idle.
                if (config.isNotebookKernelMode()) {
                    mainEvaluator.reset(static_cast<Evaluator*>(
                        CreateNotebookKernelEvaluatorDynamic(context.release(), mainEvaluatorID)));
                    break;
                }
                // Debug Adapter server: the interpreter is driven over a stdio DAP
                // frame channel, figures served headless on the same RPC dispatch.
                if (config.isDapMode()) {
                    mainEvaluator.reset(static_cast<Evaluator*>(
                        CreateDapEvaluatorDynamic(context.release(), mainEvaluatorID)));
                    break;
                }
#endif
                std::unique_ptr<AdvancedTerminal> nlsTerm;
                try {
                    nlsTerm = std::make_unique<AdvancedTerminal>();
                } catch (const std::bad_alloc&) {
                }
                if (nlsTerm) {
                    mainEvaluator = std::make_unique<Evaluator>(
                        context.release(), nlsTerm.release(), true, mainEvaluatorID);
#ifdef NELSON_GUI_ENABLED
                    if (config.isWebGraphicsMode()) {
                        StartGuiFiguresDynamic(mainEvaluator.get());
                    }
#endif
                }
                break;
            }
#ifdef NELSON_GUI_ENABLED
            case GUI: {
                WebUiLaunchOptions options;
                options.host = wstring_to_utf8(webUrl);
                options.port = webPort;
                options.hasHost = haveWebUrl;
                options.hasPort = haveWebPort;
                options.serverMode = webServerMode;
                mainEvaluator.reset(static_cast<Evaluator*>(
                    CreateGuiEvaluatorDynamic(context.get(), mainEvaluatorID, options)));
                if (mainEvaluator) {
                    context.release();
                }
                break;
            }
#endif
            default: {
                const std::string _msg = Nelson::resolveMessageTemplate(
                    "Nelson:engine:unknownEngine", "unknown engine.\n");
                fprintf(stderr, "%s", _msg.c_str());
                exit(1);
                break;
            }
            }
        }
        config.setMainEvaluator(static_cast<void*>(mainEvaluator.get()));
        Localization::Instance()->setLanguage(effectiveLang, false);
    } else {
        mainEvaluator.reset(static_cast<Evaluator*>(config.getMainEvaluator()));
    }
    return mainEvaluator.release();
}
//=============================================================================
bool
destroyMainEvaluator()
{
    auto& config = *NelsonConfiguration::getInstance();
    std::unique_ptr<Evaluator> mainEvaluator(static_cast<Evaluator*>(config.getMainEvaluator()));
    if (mainEvaluator) {
        // Detach the evaluator from the configuration before tearing anything
        // down, so the lexer's process-wide variable resolver can never reach a
        // half-destroyed evaluator/Context through getMainEvaluator().
        config.setMainEvaluator(nullptr);
        Context* ctxt = mainEvaluator->getContext();
        if (ctxt != nullptr) {
            ClearAllVariables(mainEvaluator.get());
            ClearAllGlobalVariables(mainEvaluator.get());
            // base workspace handle objects run their destructor while the
            // evaluator, its context and the modules are still alive
            DrainPendingHandleFinalizers(mainEvaluator.get());
            ModulesManager::Instance().deleteAllModules();
            delete ctxt;
            ctxt = nullptr;
            // whatever the context teardown released can no longer run user
            // code: destroy it without a destructor call
            PendingHandleFinalizers::discardAll();
        }
        PathFunctionIndexerManager::getInstance()->destroy();
        Interface* io = mainEvaluator->getInterface();
        if (io != nullptr) {
            int engineMode = config.getNelsonEngineMode();
            switch (engineMode) {
            case BASIC_ENGINE:
            case ADVANCED_ENGINE:
                break;
#ifdef NELSON_GUI_ENABLED
            case GUI: {
                delete io;
                break;
            }
#endif
            case BASIC_TERMINAL:
            case WASM_TERMINAL: {
                auto* nlsTerm = static_cast<BasicTerminal*>(io);
                delete nlsTerm;
                nlsTerm = nullptr;
                break;
            }
            case ADVANCED_TERMINAL: {
                auto* nlsTerm = static_cast<AdvancedTerminal*>(io);
                delete nlsTerm;
                nlsTerm = nullptr;
                break;
            }
            default:
                break;
            }
        }
        BuiltInFunctionDefManager::getInstance()->destroy();
        Localization::Instance()->destroy();
        config.setMainEvaluator(nullptr);
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
