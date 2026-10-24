//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsGui_exports.h"
#include <QtCore/QObject>
//=============================================================================
namespace Nelson {
//=============================================================================
// Process wide notifier used to apply preference changes live across modules.
//
// The preferences dialog lives in the gui module and cannot depend on
// text_editor (that would be a dependency cycle). Instead the dialog emits
// through this gui-owned singleton, and the text editor - which already links
// gui - connects to it to re-read its settings from QSettings on the fly.
//=============================================================================
class NLSGUI_IMPEXP GuiPreferencesNotifier : public QObject
{
    Q_OBJECT
public:
    static GuiPreferencesNotifier&
    instance();
    void
    notifyEditorPreferencesChanged();

signals:
    void
    editorPreferencesChanged();

private:
    GuiPreferencesNotifier() = default;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
