//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
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
    Evaluator* mainEvaluator = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    size_t mainEvaluatorID = 0;
    setDefaultMaxNumCompThreads();
    if (mainEvaluator == nullptr) {
        Context* context = nullptr;
        try {
            context = new Context;
        } catch (std::bad_alloc&) {
            context = nullptr;
        }
        std::wstring effectiveLang = Localization::Instance()->initializeLocalization(lang);
        NelsonConfiguration::getInstance()->setMainGuiObject(nullptr);

        if (context != nullptr) {
            std::string msg = _("This mode is not yet implemented.\n");
            switch (_mode) {
            case ADVANCED_SIO_CLIENT:
            case BASIC_ENGINE: {
                fprintf(stderr, "%s", msg.c_str());
                exit(1);
            } break;
            case ADVANCED_ENGINE: {
                InitGuiObjectsDynamic();
                fprintf(stderr, "%s", msg.c_str());
                exit(1);
            } break;
            case BASIC_SIO_CLIENT: {
                SioClientInterface* nlsTerm = nullptr;
                try {
                    nlsTerm = new SioClientInterface();
                } catch (std::bad_alloc&) {
                    nlsTerm = nullptr;
                }
                if (nlsTerm != nullptr) {
                    mainEvaluator = new Evaluator(context, nlsTerm, false, mainEvaluatorID);
                }
            } break;
            case BASIC_TERMINAL: {
                BasicTerminal* nlsTerm = nullptr;
                try {
                    nlsTerm = new BasicTerminal();
                } catch (std::bad_alloc&) {
                    nlsTerm = nullptr;
                }
                if (nlsTerm != nullptr) {
                    mainEvaluator = new Evaluator(context, nlsTerm, false, mainEvaluatorID);
                }
            } break;
            case ADVANCED_TERMINAL: {
                InitGuiObjectsDynamic();
                AdvancedTerminal* nlsTerm = nullptr;
                try {
                    nlsTerm = new AdvancedTerminal();
                } catch (std::bad_alloc&) {
                    nlsTerm = nullptr;
                }
                if (nlsTerm != nullptr) {
                    mainEvaluator = new Evaluator(context, nlsTerm, true, mainEvaluatorID);
                }
            } break;
            case GUI: {
                InitGuiObjectsDynamic();
                mainEvaluator = static_cast<Evaluator*>(CreateGuiEvaluatorDynamic(
                    (void*)context, _mode, minimizeWindow, mainEvaluatorID));
            } break;
            default: {
                std::string _msg = _("unknow engine.\n");
                fprintf(stderr, "%s", _msg.c_str());
                exit(1);
            } break;
            }
        }
        NelsonConfiguration::getInstance()->setMainEvaluator((void*)mainEvaluator);
        Localization::Instance()->setLanguage(effectiveLang, false);
    }
    return mainEvaluator;
}
//=============================================================================
bool
destroyMainEvaluator()
{
    Evaluator* mainEvaluator = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    if (mainEvaluator != nullptr) {
        Context* ctxt = mainEvaluator->getContext();
        if (ctxt != nullptr) {
            // delete all functions (builtin, macros, variables)
            ClearAllVariables(mainEvaluator);
            ClearAllGlobalVariables(mainEvaluator);
            ModulesManager::Instance().deleteAllModules();
            delete ctxt;
            ctxt = nullptr;
        }
        PathFunctionIndexerManager::getInstance()->destroy();
        Interface* io = mainEvaluator->getInterface();
        if (io != nullptr) {
            int engineMode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
            switch (engineMode) {
            case ADVANCED_SIO_CLIENT:
            case BASIC_SIO_CLIENT:
            case BASIC_ENGINE: {
            } break;
            case ADVANCED_ENGINE: {
                DestroyMainGuiObjectDynamic(nullptr);
            } break;
            case GUI: {
                DestroyMainGuiObjectDynamic((void*)io);
            } break;
            case BASIC_TERMINAL: {
                auto* nlsTerm = (BasicTerminal*)io;
                delete nlsTerm;
                nlsTerm = nullptr;
            } break;
            case ADVANCED_TERMINAL: {
                AdvancedTerminal* nlsTerm = (AdvancedTerminal*)io;
                delete nlsTerm;
                nlsTerm = nullptr;
                DestroyMainGuiObjectDynamic(nullptr);
            } break;
            default: {
            } break;
            }
        }
        BuiltInFunctionDefManager::getInstance()->destroy();
        Localization::Instance()->destroy();
        delete mainEvaluator;
        mainEvaluator = nullptr;
        NelsonConfiguration::getInstance()->setMainEvaluator(nullptr);
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
