//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "MainEvaluator.hpp"
#include "BasicTerminal.hpp"
#ifdef _MSC_VER
#include "WindowsConsole.hpp"
#include <Windows.h>
#else
#include "BsdTerminal.hpp"
#endif
#include "SioClientInterface.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Clear.hpp"
#include "ClearGlobal.hpp"
#include "ComputionalThreads.hpp"
#include "GuiTerminal.hpp"
#include "Localization.hpp"
#include "MainGuiObjectDynamic.hpp"
#include "ModulesManager.hpp"
#include "PathFuncManager.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Evaluator* mainEvaluator = nullptr;
//=============================================================================
Evaluator*
createMainEvaluator(NELSON_ENGINE_MODE _mode, const std::wstring& lang, bool minimizeWindow)
{
    setDefaultMaxNumCompThreads();
    if (mainEvaluator == nullptr) {
        Context* context = nullptr;
        try {
            context = new Context;
        } catch (std::bad_alloc&) {
            context = nullptr;
        }
        std::wstring effectiveLang = Localization::Instance()->initializeLocalization(lang);
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
                    mainEvaluator = new Evaluator(context, nlsTerm, _mode);
                    mainEvaluator->mainGuiObject = nullptr;
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
                    mainEvaluator = new Evaluator(context, nlsTerm, _mode);
                    mainEvaluator->mainGuiObject = nullptr;
                }
            } break;
            case ADVANCED_TERMINAL: {
                InitGuiObjectsDynamic();
#ifdef _MSC_VER
                WindowsConsole* nlsTerm = nullptr;
                try {
                    nlsTerm = new WindowsConsole();
                } catch (std::bad_alloc&) {
                    nlsTerm = nullptr;
                }
#else
                BsdTerminal* nlsTerm = nullptr;
                try {
                    nlsTerm = new BsdTerminal();
                } catch (std::bad_alloc&) {
                    nlsTerm = nullptr;
                }
#endif
                if (nlsTerm != nullptr) {
                    mainEvaluator = new Evaluator(context, nlsTerm, _mode);
                }
            } break;
            case GUI: {
                InitGuiObjectsDynamic();
                mainEvaluator = static_cast<Evaluator*>(
                    CreateGuiEvaluatorDynamic((void*)context, _mode, minimizeWindow));
            } break;
            default: {
                std::string _msg = _("unknow engine.\n");
                fprintf(stderr, "%s", _msg.c_str());
                exit(1);
            } break;
            }
        }
        Localization::Instance()->setLanguage(effectiveLang, false);
    }
    return mainEvaluator;
}
//=============================================================================
Evaluator*
getMainEvaluator()
{
    return mainEvaluator;
}
//=============================================================================
bool
destroyMainEvaluator()
{
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
        PathFuncManager::getInstance()->destroy();
        Interface* io = mainEvaluator->getInterface();
        if (io != nullptr) {
            int engineMode = mainEvaluator->getNelsonEngineMode();
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
#ifdef _MSC_VER
                auto* nlsTerm = (WindowsConsole*)io;
#else
                BsdTerminal* nlsTerm = (BsdTerminal*)io;
#endif
                delete nlsTerm;
                nlsTerm = nullptr;
                DestroyMainGuiObjectDynamic(nullptr);
            } break;
            default: { } break; }
        }
        BuiltInFunctionDefManager::getInstance()->destroy();
        Localization::Instance()->destroy();
        delete mainEvaluator;
        mainEvaluator = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
void*
getNelsonMainEvaluator()
{
    return (ptrBuiltin)Nelson::getMainEvaluator();
}
//=============================================================================
