//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Terminal.hpp"
#include "QtShellTerminal.h"
#include <QtWidgets/QMainWindow>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "MainGuiObject.hpp"
#include "NelSon_engine_mode.h"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "Nelson_VERSION.h"
#include "QStringConverter.hpp"
#include "RuntimeDispatch.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace {
//=============================================================================
template <typename Func>
void
runOnGuiThread(Func func)
{
    if (Nelson::hasGuiServices() && !Nelson::isGuiThread()) {
        Nelson::callGuiSync(func);
        return;
    }
    func();
}
//=============================================================================
template <typename Result, typename Func>
Result
runOnGuiThread(Func func, Result defaultValue)
{
    Result result = defaultValue;
    runOnGuiThread([&]() { result = func(); });
    return result;
}
//=============================================================================
class TerminalHandleObject : public Nelson::HandleGenericObject
{
public:
    explicit TerminalHandleObject(QtShellTerminal* terminal)
        : Nelson::HandleGenericObject(NLS_HANDLE_TERMINAL_CATEGORY_STR, terminal, true)
    {
    }

    void
    onDelete(Nelson::Evaluator*) override
    {
        auto* terminal = static_cast<QtShellTerminal*>(pointer());
        if (terminal == nullptr) {
            return;
        }
        runOnGuiThread([terminal]() {
            terminal->closeTerminal();
            terminal->deleteLater();
        });
        setPointer(nullptr);
    }
};
//=============================================================================
QtShellTerminal*
getTerminalWidget(const Nelson::ArrayOf& handle)
{
    if (!handle.isHandle() || !handle.isScalar()
        || handle.handleCategory() != NLS_HANDLE_TERMINAL_CATEGORY_STR) {
        Nelson::Error(L"Nelson:terminal:invalidHandle", L"Valid terminal handle expected.");
    }
    Nelson::HandleGenericObject* object = handle.asHandleScalar();
    auto* terminalObject = dynamic_cast<TerminalHandleObject*>(object);
    if (terminalObject == nullptr || terminalObject->pointer() == nullptr) {
        Nelson::Error(L"Nelson:terminal:invalidHandle", L"Valid terminal handle expected.");
    }
    return static_cast<QtShellTerminal*>(terminalObject->pointer());
}
//=============================================================================
Nelson::HandleRef
getHandleRef(const Nelson::ArrayOf& handle)
{
    if (!handle.isHandle() || !handle.isScalar()) {
        Nelson::Error(L"Nelson:terminal:invalidHandle", L"Valid terminal handle expected.");
    }
    const auto* refs = static_cast<const Nelson::HandleRef*>(handle.dataPointer());
    if (refs == nullptr || !refs[0] || !refs[0]->isValid()) {
        Nelson::Error(L"Nelson:terminal:invalidHandle", L"Valid terminal handle expected.");
    }
    if (refs[0]->category() != NLS_HANDLE_TERMINAL_CATEGORY_STR) {
        Nelson::Error(L"Nelson:terminal:invalidHandle", L"Valid terminal handle expected.");
    }
    return refs[0];
}
//=============================================================================
void
checkGuiMode()
{
    auto engineMode = Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (engineMode != GUI) {
        Nelson::Error(
            L"Nelson:terminal:guiModeRequired", L"Terminal is available only in GUI mode.");
    }
}
//=============================================================================
} // namespace
//=============================================================================
namespace Nelson::Terminal {
//=============================================================================
ArrayOf
create(const TerminalOptions& options)
{
    const auto mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (mode == WEB_GUI || mode == WASM_TERMINAL) {
        // In the web desktop, open a browser shell panel instead of a Qt window.
        WebDesktopBridge::getInstance()->openWebTerminal();
        return ArrayOf::characterArray(std::string());
    }
    checkGuiMode();
    return runOnGuiThread<ArrayOf>(
        [&]() {
            auto* mainWindow = static_cast<QMainWindow*>(GetMainGuiObject());
            if (mainWindow == nullptr) {
                Error(L"Nelson:terminal:mainWindowNotAvailable", L"Main GUI window not available.");
            }
            auto* terminal = new QtShellTerminal(mainWindow, options);
            mainWindow->addDockWidget(Qt::BottomDockWidgetArea, terminal);
            if (options.windowStyle == L"normal") {
                terminal->setFloating(true);
            }
            terminal->show();
            terminal->focusTerminal();
            if (!terminal->start()) {
                terminal->deleteLater();
                Error(L"Nelson:terminal:cannotStartShell", L"Cannot start terminal shell.");
            }
            return ArrayOf::handle(new TerminalHandleObject(terminal));
        },
        ArrayOf());
}
//=============================================================================
void
write(const ArrayOf& handle, const std::wstring& text)
{
    runOnGuiThread([&]() { getTerminalWidget(handle)->writeText(wstringToQString(text)); });
}
//=============================================================================
std::wstring
read(const ArrayOf& handle)
{
    return runOnGuiThread<std::wstring>(
        [&]() { return QStringTowstring(getTerminalWidget(handle)->readText()); }, L"");
}
//=============================================================================
void
close(Evaluator* eval, const ArrayOf& handle)
{
    HandleRef ref = getHandleRef(handle);
    ref->invalidate(eval);
}
//=============================================================================
void
focus(const ArrayOf& handle)
{
    runOnGuiThread([&]() { getTerminalWidget(handle)->focusTerminal(); });
}
//=============================================================================
void
clear(const ArrayOf& handle)
{
    runOnGuiThread([&]() { getTerminalWidget(handle)->clearTerminal(); });
}
//=============================================================================
ArrayOf
get(const ArrayOf& handle, const std::wstring& propertyName)
{
    return runOnGuiThread<ArrayOf>(
        [&]() {
            QtShellTerminal* terminal = getTerminalWidget(handle);
            if (propertyName == L"Name") {
                return ArrayOf::characterArray(QStringTowstring(terminal->terminalName()));
            }
            if (propertyName == L"Shell") {
                return ArrayOf::characterArray(QStringTowstring(terminal->terminalShell()));
            }
            if (propertyName == L"Place") {
                return ArrayOf::characterArray("nelson");
            }
            if (propertyName == L"WindowStyle") {
                return ArrayOf::characterArray(QStringTowstring(terminal->windowStyle()));
            }
            if (propertyName == L"Theme") {
                return ArrayOf::characterArray(QStringTowstring(terminal->theme()));
            }
            if (propertyName == L"Running") {
                return ArrayOf::logicalScalar(terminal->isRunning());
            }
            if (propertyName == L"ExitCode") {
                return ArrayOf::doubleScalar(static_cast<double>(terminal->exitCode()));
            }
            if (propertyName == L"ProcessId") {
                return ArrayOf::doubleScalar(static_cast<double>(terminal->processId()));
            }
            Error(L"Nelson:terminal:unsupportedProperty", L"Unsupported terminal property.");
            return ArrayOf();
        },
        ArrayOf());
}
//=============================================================================
void
set(const ArrayOf& handle, const std::wstring& propertyName, const ArrayOf& value)
{
    runOnGuiThread([&]() {
        QtShellTerminal* terminal = getTerminalWidget(handle);
        if (propertyName == L"Name") {
            terminal->setTerminalName(wstringToQString(value.asWideString()));
            return;
        }
        if (propertyName == L"Theme") {
            terminal->setTerminalTheme(wstringToQString(value.asWideString()));
            return;
        }
        Error(L"Nelson:terminal:unsupportedWritableProperty",
            L"Unsupported writable terminal property.");
    });
}
//=============================================================================
ArrayOf
list()
{
    std::vector<HandleRef> handles = HandleManager::getInstance()->getAllHandleRefsOfCategory(
        NLS_HANDLE_TERMINAL_CATEGORY_STR);
    size_t nbHandles = handles.size();
    if (nbHandles == 0) {
        Dimensions dims(0, 0);
        return ArrayOf::emptyHandle(NLS_HANDLE_TERMINAL_CATEGORY_STR, dims);
    }
    Dimensions dims(1, nbHandles);
    auto* refs = static_cast<HandleRef*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, nbHandles, stringVector(), false));
    for (size_t k = 0; k < nbHandles; ++k) {
        refs[k] = handles[k];
    }
    return ArrayOf(NLS_HANDLE, dims, refs);
}
//=============================================================================
void
closeAll(Evaluator* eval)
{
    std::vector<HandleRef> handles = HandleManager::getInstance()->getAllHandleRefsOfCategory(
        NLS_HANDLE_TERMINAL_CATEGORY_STR);
    for (const HandleRef& ref : handles) {
        if (ref) {
            ref->invalidate(eval);
        }
    }
}
//=============================================================================
wstringVector
themes()
{
    return { L"auto", L"light", L"dark" };
}
//=============================================================================
std::wstring
version()
{
    return utf8_to_wstring(std::string(NELSON_VERSION_STRING));
}
//=============================================================================
} // namespace Nelson::Terminal
//=============================================================================
