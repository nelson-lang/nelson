//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory>
#include "AdvancedTerminal.hpp"
#include "MainEvaluator.hpp"
#include "BasicTerminal.hpp"
#include "SioClientInterface.hpp"
#include "BuiltInFunctionDefManager.hpp"
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
//=============================================================================
namespace Nelson {
//=============================================================================
Evaluator*
createMainEvaluator(NELSON_ENGINE_MODE _mode, const std::wstring& lang, bool minimizeWindow)
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
            const std::string msg = _("This mode is not yet implemented.\n");
            switch (_mode) {
            case ADVANCED_SIO_CLIENT:
            case BASIC_ENGINE:
                fprintf(stderr, "%s", msg.c_str());
                exit(1);
                break;
            case ADVANCED_ENGINE:
                InitGuiObjectsDynamic();
                fprintf(stderr, "%s", msg.c_str());
                exit(1);
                break;
            case BASIC_SIO_CLIENT: {
                std::unique_ptr<SioClientInterface> nlsTerm;
                try {
                    nlsTerm = std::make_unique<SioClientInterface>();
                } catch (const std::bad_alloc&) {
                }
                if (nlsTerm) {
                    mainEvaluator = std::make_unique<Evaluator>(
                        context.release(), nlsTerm.release(), false, mainEvaluatorID);
                }
                break;
            }
            case BASIC_TERMINAL: {
                std::unique_ptr<BasicTerminal> nlsTerm;
                try {
                    nlsTerm = std::make_unique<BasicTerminal>();
                } catch (const std::bad_alloc&) {
                }
                if (nlsTerm) {
                    mainEvaluator = std::make_unique<Evaluator>(
                        context.release(), nlsTerm.release(), false, mainEvaluatorID);
                }
                break;
            }
            case ADVANCED_TERMINAL: {
                InitGuiObjectsDynamic();
                std::unique_ptr<AdvancedTerminal> nlsTerm;
                try {
                    nlsTerm = std::make_unique<AdvancedTerminal>();
                } catch (const std::bad_alloc&) {
                }
                if (nlsTerm) {
                    mainEvaluator = std::make_unique<Evaluator>(
                        context.release(), nlsTerm.release(), true, mainEvaluatorID);
                }
                break;
            }
            case GUI:
                InitGuiObjectsDynamic();
                mainEvaluator = std::unique_ptr<Evaluator>(static_cast<Evaluator*>(
                    CreateGuiEvaluatorDynamic(static_cast<void*>(context.release()), _mode,
                        minimizeWindow, mainEvaluatorID)));
                break;
            default: {
                const std::string _msg = _("unknow engine.\n");
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
        Context* ctxt = mainEvaluator->getContext();
        if (ctxt != nullptr) {
            ClearAllVariables(mainEvaluator.get());
            ClearAllGlobalVariables(mainEvaluator.get());
            ModulesManager::Instance().deleteAllModules();
            delete ctxt;
            ctxt = nullptr;
        }
        PathFunctionIndexerManager::getInstance()->destroy();
        Interface* io = mainEvaluator->getInterface();
        if (io != nullptr) {
            int engineMode = config.getNelsonEngineMode();
            switch (engineMode) {
            case ADVANCED_SIO_CLIENT:
            case BASIC_SIO_CLIENT:
            case BASIC_ENGINE:
                break;
            case ADVANCED_ENGINE:
                DestroyMainGuiObjectDynamic(nullptr);
                break;
            case GUI:
                DestroyMainGuiObjectDynamic(static_cast<void*>(io));
                break;
            case BASIC_TERMINAL: {
                auto* nlsTerm = static_cast<BasicTerminal*>(io);
                delete nlsTerm;
                nlsTerm = nullptr;
                break;
            }
            case ADVANCED_TERMINAL: {
                auto* nlsTerm = static_cast<AdvancedTerminal*>(io);
                delete nlsTerm;
                nlsTerm = nullptr;
                DestroyMainGuiObjectDynamic(nullptr);
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
