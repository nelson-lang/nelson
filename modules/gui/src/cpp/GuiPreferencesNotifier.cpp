//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GuiPreferencesNotifier.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GuiPreferencesNotifier&
GuiPreferencesNotifier::instance()
{
    static GuiPreferencesNotifier theInstance;
    return theInstance;
}
//=============================================================================
void
GuiPreferencesNotifier::notifyEditorPreferencesChanged()
{
    emit editorPreferencesChanged();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
// The Q_OBJECT class lives in an exported header under src/include (so that
// text_editor can connect to it); AUTOMOC only auto-generates the meta object
// for headers next to a .cpp, so it is pulled in explicitly here.
#include "moc_GuiPreferencesNotifier.cpp"
//=============================================================================
