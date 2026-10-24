//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QSettings>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFontComboBox>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QVBoxLayout>
#include "QtPreferencesDialog.h"
#include "QtTerminal.h"
#include "QtLookAndFeel.hpp"
#include "NelsonPalette.hpp"
#include "DefaultFont.hpp"
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "NelsonEditorSettings.hpp"
#include "GuiPreferencesNotifier.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Keys must match the readers scattered in the gui module.
static const QString SETTING_DEFAULT_FONT_NAME = "Font/Name";
static const QString SETTING_DEFAULT_FONT_SIZE = "Font/Size";
static const QString SETTING_COMMAND_WINDOW_FONT_SIZE = "command_window_font_size";
static const QString SETTING_COMMAND_WINDOW_TRUNCATE_MATRICES = "command_window_truncate_matrices";
static const QString SETTING_COMMAND_WINDOW_WRAP_LINES = "command_window_wrap_lines";
static const QString SETTING_FIGURE_DOCK_NEW = "figure_DockNewInDesktop";
//=============================================================================
constexpr int FONT_MIN_SIZE = 5;
constexpr int FONT_MAX_SIZE = 72;
constexpr int COMMAND_WINDOW_FONT_MIN_SIZE = 4;
constexpr int COMMAND_WINDOW_FONT_MAX_SIZE = 72;
constexpr int DEFAULT_FONT_SIZE = 10;
//=============================================================================
QtPreferencesDialog::QtPreferencesDialog(QtTerminal* terminal, QWidget* parent)
    : QDialog(parent)
    , m_terminal(terminal)
    , m_tabs(nullptr)
    , m_themeCombo(nullptr)
    , m_dockNewFigures(nullptr)
    , m_styleCombo(nullptr)
    , m_appFontCombo(nullptr)
    , m_appFontSize(nullptr)
    , m_commandWindowFontSize(nullptr)
    , m_truncateMatrices(nullptr)
    , m_wrapLines(nullptr)
    , m_editorFontCombo(nullptr)
    , m_editorFontSize(nullptr)
    , m_editorIndentSize(nullptr)
    , m_editorCodeAnalyzer(nullptr)
{
    setWindowTitle(TR("Preferences"));
    setObjectName("NelsonPreferencesDialog");
    setModal(true);

    QVBoxLayout* mainLayout = new QVBoxLayout(this);
    m_tabs = new QTabWidget(this);
    m_tabs->addTab(createGeneralTab(), TR("General"));
    m_tabs->addTab(createFontsTab(), TR("Fonts"));
    m_tabs->addTab(createCommandWindowTab(), TR("Command Window"));
    m_tabs->addTab(createEditorTab(), TR("Editor"));
    mainLayout->addWidget(m_tabs);

    QDialogButtonBox* buttons = new QDialogButtonBox(
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel | QDialogButtonBox::Apply, this);
    mainLayout->addWidget(buttons);
    connect(buttons, &QDialogButtonBox::accepted, this, &QtPreferencesDialog::onAccept);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    connect(buttons->button(QDialogButtonBox::Apply), &QPushButton::clicked, this,
        &QtPreferencesDialog::onApply);

    loadSettings();
    resize(460, 360);
}
//=============================================================================
void
QtPreferencesDialog::selectPage(const QString& page)
{
    QString requested = page.trimmed();
    if (requested.isEmpty() || m_tabs == nullptr) {
        return;
    }
    requested.remove(' ');
    requested.remove('_');
    requested.remove('-');
    requested = requested.toLower();

    for (int k = 0; k < m_tabs->count(); ++k) {
        QString tabName = m_tabs->tabText(k);
        tabName.remove(' ');
        tabName.remove('_');
        tabName.remove('-');
        if (tabName.toLower() == requested) {
            m_tabs->setCurrentIndex(k);
            return;
        }
    }
}
//=============================================================================
QWidget*
QtPreferencesDialog::createGeneralTab()
{
    QWidget* tab = new QWidget(this);
    QFormLayout* layout = new QFormLayout(tab);

    m_themeCombo = new QComboBox(tab);
    m_themeCombo->addItem(TR("System default"), QString("system"));
    m_themeCombo->addItem(TR("Light"), QString("light"));
    m_themeCombo->addItem(TR("Dark"), QString("dark"));
    m_themeCombo->setToolTip(
        TR("Color theme of the desktop ('System default' follows the operating system)."));
    layout->addRow(TR("Theme:"), m_themeCombo);

    m_styleCombo = new QComboBox(tab);
    wstringVector styles = GetLookAndFeelAvailable();
    for (const std::wstring& s : styles) {
        m_styleCombo->addItem(wstringToQString(s));
    }
    m_styleCombo->setToolTip(TR("Widget style used by the whole application."));
    layout->addRow(TR("Look and feel:"), m_styleCombo);

    m_dockNewFigures = new QCheckBox(TR("Open new figures docked in the desktop"), tab);
    m_dockNewFigures->setToolTip(
        TR("New figures open as tabs docked in the Nelson desktop instead of separate windows."));
    layout->addRow(m_dockNewFigures);

    return tab;
}
//=============================================================================
QWidget*
QtPreferencesDialog::createFontsTab()
{
    QWidget* tab = new QWidget(this);
    QFormLayout* layout = new QFormLayout(tab);

    m_appFontCombo = new QFontComboBox(tab);
    m_appFontCombo->setFontFilters(QFontComboBox::MonospacedFonts);
    m_appFontCombo->setToolTip(TR("Font used by the command window and the variables editor."));
    layout->addRow(TR("Application font:"), m_appFontCombo);

    m_appFontSize = new QSpinBox(tab);
    m_appFontSize->setRange(FONT_MIN_SIZE, FONT_MAX_SIZE);
    layout->addRow(TR("Application font size:"), m_appFontSize);

    m_commandWindowFontSize = new QSpinBox(tab);
    m_commandWindowFontSize->setRange(COMMAND_WINDOW_FONT_MIN_SIZE, COMMAND_WINDOW_FONT_MAX_SIZE);
    m_commandWindowFontSize->setToolTip(
        TR("Font size of the command window (overrides the application font size)."));
    layout->addRow(TR("Command window font size:"), m_commandWindowFontSize);

    QLabel* note
        = new QLabel(TR("The application font family is applied to newly opened windows."), tab);
    note->setWordWrap(true);
    QFont noteFont = note->font();
    noteFont.setItalic(true);
    note->setFont(noteFont);
    layout->addRow(note);

    return tab;
}
//=============================================================================
QWidget*
QtPreferencesDialog::createCommandWindowTab()
{
    QWidget* tab = new QWidget(this);
    QVBoxLayout* layout = new QVBoxLayout(tab);

    m_truncateMatrices = new QCheckBox(TR("Truncate large matrices"), tab);
    m_truncateMatrices->setToolTip(TR("Show a shortened, expandable form of very large matrices."));
    layout->addWidget(m_truncateMatrices);

    m_wrapLines = new QCheckBox(TR("Wrap long lines"), tab);
    m_wrapLines->setToolTip(TR("Wrap lines that are wider than the command window."));
    layout->addWidget(m_wrapLines);

    layout->addStretch(1);
    return tab;
}
//=============================================================================
QWidget*
QtPreferencesDialog::createEditorTab()
{
    QWidget* tab = new QWidget(this);
    QFormLayout* layout = new QFormLayout(tab);

    m_editorFontCombo = new QFontComboBox(tab);
    m_editorFontCombo->setFontFilters(QFontComboBox::MonospacedFonts);
    m_editorFontCombo->setToolTip(TR("Font used by the text editor."));
    layout->addRow(TR("Editor font:"), m_editorFontCombo);

    m_editorFontSize = new QSpinBox(tab);
    m_editorFontSize->setRange(FONT_MIN_SIZE, FONT_MAX_SIZE);
    layout->addRow(TR("Editor font size:"), m_editorFontSize);

    m_editorIndentSize = new QSpinBox(tab);
    m_editorIndentSize->setRange(NELSON_EDITOR_MIN_INDENT_SIZE, NELSON_EDITOR_MAX_INDENT_SIZE);
    m_editorIndentSize->setToolTip(TR("Number of spaces used for one indentation level."));
    layout->addRow(TR("Indent size:"), m_editorIndentSize);

    m_editorCodeAnalyzer = new QCheckBox(TR("Enable code analyzer"), tab);
    m_editorCodeAnalyzer->setToolTip(
        TR("Underline suspicious code and show diagnostics while editing."));
    layout->addRow(m_editorCodeAnalyzer);

    QLabel* note = new QLabel(TR("Editor settings apply to newly opened editor windows."), tab);
    note->setWordWrap(true);
    QFont noteFont = note->font();
    noteFont.setItalic(true);
    note->setFont(noteFont);
    layout->addRow(note);

    return tab;
}
//=============================================================================
void
QtPreferencesDialog::loadSettings()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);

    int themeIdx = m_themeCombo->findData(wstringToQString(getThemePreference()));
    if (themeIdx >= 0) {
        m_themeCombo->setCurrentIndex(themeIdx);
    }

    QString currentStyle = wstringToQString(GetCurrentLookAndFeel());
    int styleIdx = m_styleCombo->findText(currentStyle, Qt::MatchFixedString);
    if (styleIdx >= 0) {
        m_styleCombo->setCurrentIndex(styleIdx);
    }

    m_dockNewFigures->setChecked(settings.value(SETTING_FIGURE_DOCK_NEW, false).toBool());

    QString appFontName = settings.value(SETTING_DEFAULT_FONT_NAME, QString()).toString().trimmed();
    if (appFontName.isEmpty()) {
        appFontName = wstringToQString(getDefaultFontName());
    }
    m_appFontCombo->setCurrentFont(QFont(appFontName));

    int appFontSize = settings.value(SETTING_DEFAULT_FONT_SIZE, DEFAULT_FONT_SIZE).toInt();
    if (appFontSize < FONT_MIN_SIZE || appFontSize > FONT_MAX_SIZE) {
        appFontSize = DEFAULT_FONT_SIZE;
    }
    m_appFontSize->setValue(appFontSize);

    int cwFontSize = settings.value(SETTING_COMMAND_WINDOW_FONT_SIZE, appFontSize).toInt();
    if (cwFontSize < COMMAND_WINDOW_FONT_MIN_SIZE || cwFontSize > COMMAND_WINDOW_FONT_MAX_SIZE) {
        cwFontSize = DEFAULT_FONT_SIZE;
    }
    m_commandWindowFontSize->setValue(cwFontSize);

    m_truncateMatrices->setChecked(
        settings.value(SETTING_COMMAND_WINDOW_TRUNCATE_MATRICES, true).toBool());
    m_wrapLines->setChecked(settings.value(SETTING_COMMAND_WINDOW_WRAP_LINES, false).toBool());

    QString editorFontName
        = settings.value(NELSON_EDITOR_SETTING_FONT_NAME, QString()).toString().trimmed();
    if (editorFontName.isEmpty()) {
        editorFontName = wstringToQString(getDefaultFontName());
    }
    m_editorFontCombo->setCurrentFont(QFont(editorFontName));

    int editorFontSize
        = settings.value(NELSON_EDITOR_SETTING_FONT_SIZE, getDefaultFontSize()).toInt();
    if (editorFontSize < FONT_MIN_SIZE || editorFontSize > FONT_MAX_SIZE) {
        editorFontSize = getDefaultFontSize();
    }
    m_editorFontSize->setValue(editorFontSize);

    int indentSize
        = settings.value(NELSON_EDITOR_SETTING_INDENT_SIZE, NELSON_EDITOR_DEFAULT_INDENT_SIZE)
              .toInt();
    if (indentSize < NELSON_EDITOR_MIN_INDENT_SIZE || indentSize > NELSON_EDITOR_MAX_INDENT_SIZE) {
        indentSize = NELSON_EDITOR_DEFAULT_INDENT_SIZE;
    }
    m_editorIndentSize->setValue(indentSize);

    m_editorCodeAnalyzer->setChecked(
        settings.value(NELSON_EDITOR_SETTING_CODE_ANALYZER_ENABLED, true).toBool());
}
//=============================================================================
void
QtPreferencesDialog::applySettings()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);

    settings.setValue(SETTING_DEFAULT_FONT_NAME, m_appFontCombo->currentFont().family());
    settings.setValue(SETTING_DEFAULT_FONT_SIZE, m_appFontSize->value());
    settings.setValue(SETTING_COMMAND_WINDOW_FONT_SIZE, m_commandWindowFontSize->value());
    settings.setValue(SETTING_COMMAND_WINDOW_TRUNCATE_MATRICES, m_truncateMatrices->isChecked());
    settings.setValue(SETTING_COMMAND_WINDOW_WRAP_LINES, m_wrapLines->isChecked());
    settings.setValue(SETTING_FIGURE_DOCK_NEW, m_dockNewFigures->isChecked());

    // Editor (read back by the text_editor module when a window opens)
    settings.setValue(NELSON_EDITOR_SETTING_FONT_NAME, m_editorFontCombo->currentFont().family());
    settings.setValue(NELSON_EDITOR_SETTING_FONT_SIZE, m_editorFontSize->value());
    settings.setValue(NELSON_EDITOR_SETTING_FONT_FIXED_PITCH, true);
    settings.setValue(NELSON_EDITOR_SETTING_INDENT_SIZE, m_editorIndentSize->value());
    settings.setValue(
        NELSON_EDITOR_SETTING_CODE_ANALYZER_ENABLED, m_editorCodeAnalyzer->isChecked());
    settings.sync();

    setThemePreference(QStringTowstring(m_themeCombo->currentData().toString()));

    QString selectedStyle = m_styleCombo->currentText();
    if (!selectedStyle.isEmpty()
        && selectedStyle.compare(wstringToQString(GetCurrentLookAndFeel()), Qt::CaseInsensitive)
            != 0) {
        SetCurrentLookAndFeel(QStringTowstring(selectedStyle));
    }

    // Refresh the cached default font, then let the command window re-read
    // everything (family + size + wrap + truncate) from the settings.
    reloadDefaultFontSettings();
    if (m_terminal) {
        m_terminal->reloadPreferences();
    }
    GuiPreferencesNotifier::instance().notifyEditorPreferencesChanged();
}
//=============================================================================
void
QtPreferencesDialog::onApply()
{
    applySettings();
}
//=============================================================================
void
QtPreferencesDialog::onAccept()
{
    applySettings();
    accept();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
