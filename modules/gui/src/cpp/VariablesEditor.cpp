//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariablesEditor.hpp"
#include "QtVariablesEditor.h"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include <mutex>
//=============================================================================
namespace Nelson::VariablesEditor {
//=============================================================================
class VariablesEditorSingleton
{
private:
    static std::mutex mutex_;
    static QtVariablesEditor* instance_;

public:
    static QtVariablesEditor*
    instance(Context* context = nullptr)
    {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!instance_ && context) {
            instance_ = new QtVariablesEditor(nullptr);
            instance_->setContext(context);
            instance_->restorePosition();
        }
        return instance_;
    }

    static bool
    destroy()
    {
        std::lock_guard<std::mutex> lock(mutex_);
        if (instance_) {
            instance_->savePositionAndVisibility();
            instance_->deleteLater();
            instance_ = nullptr;
            return true;
        }
        return false;
    }

    static QtVariablesEditor*
    getRef()
    {
        std::lock_guard<std::mutex> lock(mutex_);
        return instance_;
    }
};

// Static member definitions
std::mutex VariablesEditorSingleton::mutex_;
QtVariablesEditor* VariablesEditorSingleton::instance_ = nullptr;

//=============================================================================
void*
getVariablesEditor()
{
    return VariablesEditorSingleton::getRef();
}
//=============================================================================
bool
isVariablesEditorVisible()
{
    auto* editor = VariablesEditorSingleton::getRef();
    return editor ? editor->isVisible() : false;
}
//=============================================================================
void
showVariablesEditor()
{
    auto* editor = VariablesEditorSingleton::getRef();
    if (editor) {
        editor->show();
        editor->activateWindow();
        editor->raise();
    }
}
//=============================================================================
void
hideVariablesEditor()
{
    auto* editor = VariablesEditorSingleton::getRef();
    if (editor) {
        editor->hide();
    }
}
//=============================================================================
void
toggleVisibilityVariablesEditor()
{
    auto* editor = VariablesEditorSingleton::getRef();
    if (editor) {
        if (editor->isVisible()) {
            editor->hide();
        } else {
            editor->show();
            editor->activateWindow();
            editor->raise();
        }
    }
}
//=============================================================================
bool
createVariablesEditor(Context* context)
{
    return (VariablesEditorSingleton::instance(context) != nullptr);
}
//=============================================================================
bool
destroyVariablesEditor()
{
    auto* editor = VariablesEditorSingleton::getRef();
    if (editor) {
        return VariablesEditorSingleton::destroy();
    }
    return true;
}
//=============================================================================
bool
updateVariablesEditor()
{
    auto* editor = VariablesEditorSingleton::getRef();
    if (editor) {
        editor->updateVariables();
        return true;
    }
    return false;
}
//=============================================================================
bool
openVariable(const std::wstring& variableName)
{
    auto* editor = VariablesEditorSingleton::getRef();
    if (editor) {
        return editor->openVariable(wstringToQString(variableName));
    }

    return false;
}
//=============================================================================
} // namespace Nelson::VariablesEditor
//=============================================================================
