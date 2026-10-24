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
#ifdef NELSON_WEB_GUI_ENABLED
#include "MainWebGuiDynamic.hpp"
#include "WebUiHelpers.hpp"
#endif
#include "BuiltInFunctionDefManager.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "Clear.hpp"
#include "ClearGlobal.hpp"
#include "ComputionalThreads.hpp"
#include "GuiTerminal.hpp"
#include "Localization.hpp"
#include "MainGuiObjectDynamic.hpp"
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
// Whether the process can talk to a window server. Graphics availability follows
// the display, not the terminal: redirected standard streams still draw figures.
static bool
isDisplayAvailable()
{
#if defined(_WIN32) || defined(__APPLE__)
    return true;
#else
    return (getenv("DISPLAY") != nullptr) || (getenv("WAYLAND_DISPLAY") != nullptr);
#endif
}
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
                InitGuiObjectsDynamic();
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
                if (isDisplayAvailable()) {
                    InitGuiObjectsDynamic();
                }
                std::unique_ptr<AdvancedTerminal> nlsTerm;
                try {
                    nlsTerm = std::make_unique<AdvancedTerminal>();
                } catch (const std::bad_alloc&) {
                }
                if (nlsTerm) {
                    mainEvaluator = std::make_unique<Evaluator>(
                        context.release(), nlsTerm.release(), true, mainEvaluatorID);
#ifdef NELSON_WEB_GUI_ENABLED
                    if (config.isWebGraphicsMode()) {
                        StartWebGuiFiguresDynamic(mainEvaluator.get());
                    }
#endif
                }
                break;
            }
            case GUI:
                InitGuiObjectsDynamic();
                mainEvaluator = std::unique_ptr<Evaluator>(static_cast<Evaluator*>(
                    CreateGuiEvaluatorDynamic(static_cast<void*>(context.release()), _mode,
                        minimizeWindow, mainEvaluatorID)));
                break;
#ifdef NELSON_WEB_GUI_ENABLED
            case WEB_GUI: {
                WebUiLaunchOptions options;
                options.host = wstring_to_utf8(webUrl);
                options.port = webPort;
                options.hasHost = haveWebUrl;
                options.hasPort = haveWebPort;
                options.serverMode = webServerMode;
                mainEvaluator.reset(static_cast<Evaluator*>(
                    CreateWebGuiEvaluatorDynamic(context.get(), mainEvaluatorID, options)));
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
                break;
            case ADVANCED_ENGINE:
                DestroyMainGuiObjectDynamic(nullptr);
                break;
            case GUI:
                DestroyMainGuiObjectDynamic(static_cast<void*>(io));
                break;
#ifdef NELSON_WEB_GUI_ENABLED
            case WEB_GUI: {
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
                if (hasGuiServices()) {
                    DestroyMainGuiObjectDynamic(nullptr);
                }
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
