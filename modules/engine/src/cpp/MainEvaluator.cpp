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
#include "MainEvaluator.hpp"
#include "BasicTerminal.hpp"
#ifdef _MSC_VER
#include "WindowsConsole.hpp"
#include <Windows.h>
#else
#include "BsdTerminal.hpp"
#endif
#include "AstManager.hpp"
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
createMainEvaluator(Interface* io, NELSON_ENGINE_MODE _mode, std::wstring lang)
{
    if (io) {
        if (mainEvaluator == nullptr) {
            std::wstring effectiveLang = Localization::Instance()->initializeLocalization(lang);
            Context* context = new Context;
            if (context) {
                setDefaultMaxNumCompThreads();
                mainEvaluator = new Evaluator(context, io, _mode);
                Localization::Instance()->setLanguage(effectiveLang, false);
            }
        }
    }
    return mainEvaluator;
}
//=============================================================================
Evaluator*
createMainEvaluator(NELSON_ENGINE_MODE _mode, std::wstring lang)
{
    setDefaultMaxNumCompThreads();
    if (mainEvaluator == nullptr) {
        Context* context = new Context;
        std::wstring effectiveLang = Localization::Instance()->initializeLocalization(lang);
        if (context) {
            switch (_mode) {
            case BASIC_ENGINE: {
                fprintf(stderr, _("This mode is not yet implemented.\n").c_str());
                exit(1);
            } break;
            case ADVANCED_ENGINE: {
                InitGuiObjectsDynamic();
                fprintf(stderr, "%s", _("This mode is not yet implemented.\n").c_str());
                exit(1);
            } break;
            case BASIC_TERMINAL: {
                BasicTerminal* nlsTerm = new BasicTerminal();
                if (nlsTerm) {
                    mainEvaluator = new Evaluator(context, nlsTerm, _mode);
                    mainEvaluator->mainGuiObject = nullptr;
                }
            } break;
            case ADVANCED_TERMINAL: {
                InitGuiObjectsDynamic();
#ifdef _MSC_VER
                WindowsConsole* nlsTerm = new WindowsConsole();
#else
                BsdTerminal* nlsTerm = new BsdTerminal();
#endif
                if (nlsTerm) {
                    mainEvaluator = new Evaluator(context, nlsTerm, _mode);
                }
            } break;
            case GUI: {
                InitGuiObjectsDynamic();
                mainEvaluator = (Evaluator*)CreateGuiEvaluatorDynamic((void*)context, _mode);
            } break;
            default: {
                fprintf(stderr, "%s", _("unknow engine.\n").c_str());
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
    if (mainEvaluator) {
        Context* ctxt = mainEvaluator->getContext();
        if (ctxt) {
            // delete all functions (builtin, macros, variables)
            ClearAllVariables(mainEvaluator);
            ClearAllGlobalVariables(mainEvaluator);
            ModulesManager::Instance().deleteAllModules();
            delete ctxt;
            ctxt = nullptr;
        }
        Interface* io = mainEvaluator->getInterface();
        if (io) {
            int engineMode = mainEvaluator->getNelsonEngineMode();
            switch (engineMode) {
            case BASIC_ENGINE: {
            } break;
            case ADVANCED_ENGINE: {
                DestroyMainGuiObjectDynamic(nullptr);
            } break;
            case GUI: {
                DestroyMainGuiObjectDynamic((void*)io);
            } break;
            case BASIC_TERMINAL: {
                BasicTerminal* nlsTerm = (BasicTerminal*)io;
                delete nlsTerm;
                nlsTerm = nullptr;
            } break;
            case ADVANCED_TERMINAL: {
#ifdef _MSC_VER
                WindowsConsole* nlsTerm = (WindowsConsole*)io;
#else
                BsdTerminal* nlsTerm = (BsdTerminal*)io;
#endif
                delete nlsTerm;
                nlsTerm = nullptr;
                DestroyMainGuiObjectDynamic(nullptr);
            } break;
            default: { } break; }
        }
        PathFuncManager::getInstance()->destroy();
        BuiltInFunctionDefManager::getInstance()->destroy();
        Localization::Instance()->destroy();
        delete mainEvaluator;
        mainEvaluator = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
void*
getNelsonMainEvaluator()
{
    return (void*)Nelson::getMainEvaluator();
}
//=============================================================================
