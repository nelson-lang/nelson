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
#include <QtWidgets/QDialog>
//=============================================================================
class QComboBox;
class QFontComboBox;
class QSpinBox;
class QCheckBox;
class QTabWidget;
class QtTerminal;
//=============================================================================
namespace Nelson {
//=============================================================================
// Central preferences dialog: the command window and appearance settings in one tabbed
// window. Writes to the shared QSettings scope and applies live.
class QtPreferencesDialog : public QDialog
{
    Q_OBJECT
public:
    // 'terminal' is the running command window, used to apply the command
    // window settings live; it may be null.
    explicit QtPreferencesDialog(QtTerminal* terminal, QWidget* parent = nullptr);
    void
    selectPage(const QString& page);

private slots:
    void
    onAccept();
    void
    onApply();

private:
    QWidget*
    createGeneralTab();
    QWidget*
    createFontsTab();
    QWidget*
    createCommandWindowTab();
    QWidget*
    createEditorTab();
    void
    loadSettings();
    void
    applySettings();

    QtTerminal* m_terminal;
    QTabWidget* m_tabs;

    QComboBox* m_themeCombo;
    QCheckBox* m_dockNewFigures;
    QComboBox* m_styleCombo;
    QFontComboBox* m_appFontCombo;
    QSpinBox* m_appFontSize;
    QSpinBox* m_commandWindowFontSize;
    QCheckBox* m_truncateMatrices;
    QCheckBox* m_wrapLines;

    QFontComboBox* m_editorFontCombo;
    QSpinBox* m_editorFontSize;
    QSpinBox* m_editorIndentSize;
    QCheckBox* m_editorCodeAnalyzer;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
