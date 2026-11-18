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
#include <QtCore/QList>
#include <QtCore/QMap>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QTimer>
#include <QtCore/QUrl>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QTextBrowser>
//=============================================================================
// Dockable assistant panel for the Qt desktop (nelson-gui). It mirrors the web
// desktop assistant pane: a model selector, chat bubbles, fenced code blocks
// with Copy / Run, live "thinking" streaming and a Stop control. Work is queued
// on the interpreter (non-blocking); streamed tokens and the final answer arrive
// through the assistant sink, marshalled to the GUI thread by a queued signal.
class NLSGUI_IMPEXP QtAssistantBrowser : public QDockWidget
{
    Q_OBJECT
public:
    QtAssistantBrowser(QWidget* parent = nullptr);
    ~QtAssistantBrowser() override;

    // Re-read the shared conversation (seeds only an empty, idle dock), so the dock
    // catches up after a talk continued in another GUI.
    void
    refreshHistory();

signals:
    void
    assistantEvent(const QString& channel, const QString& payload);
    // A host action asked the main window to bring a GUI panel (e.g. the profiler)
    // to the front; the main window, which owns the docks, acts on it.
    void
    requestShowPanel(const QString& panel);

protected:
    void
    changeEvent(QEvent* event) override;
    // Refresh the model list when the selector is opened, so a provider that
    // became reachable after startup (e.g. a local Ollama) shows up.
    bool
    eventFilter(QObject* watched, QEvent* event) override;

private slots:
    void
    onSend();
    // Clear the on-screen transcript and start a fresh conversation (forgets the
    // shared stored history and resets the live agent).
    void
    onNewConversation();
    void
    onAssistantEvent(const QString& channel, const QString& payload);
    void
    onAnchorClicked(const QUrl& url);
    void
    onPollTick();

private:
    struct ChatMessage
    {
        QString role; // "user" or "assistant"
        QString text;
    };

    // Colours derived from the widget QPalette so the panel matches the active
    // theme (light / dark) like every other dock, instead of hard-coded values.
    struct Theme
    {
        QString text;
        QString muted;
        QString userBg;
        QString userText;
        QString cardBg;
        QString border;
        QString codeBg;
        QString link;
    };

    void
    updateTheme();
    void
    requestModels();
    // Load this dock's ('qt' surface) stored conversation so a restart restores
    // the prior turns; the result comes back as a 'history' assistant event.
    void
    requestHistory();
    void
    submitPrompt(const QString& prompt);
    void
    rebuild();
    QString
    renderMessages();
    QString
    renderBubble(const QString& text);
    QString
    renderInline(const QString& text) const;
    static QString
    escapeHtml(const QString& text);

    Theme m_theme;

    QComboBox* m_model = nullptr;
    QComboBox* m_permMode = nullptr;
    QTextBrowser* m_output = nullptr;
    QLineEdit* m_input = nullptr;
    QPushButton* m_send = nullptr;
    QPushButton* m_newChat = nullptr;
    // Drives the claude-agent-acp poll from the GUI side, so the reply does not
    // depend on a Nelson timer firing at this dock's prompt.
    QTimer* m_pollTimer = nullptr;
    // Short repeated retry to load the shared history at startup: the first read can
    // run before the engine is ready (returns empty); stops once the dock is seeded.
    QTimer* m_historyRetryTimer = nullptr;
    int m_historyRetryAttempts = 0;

    QList<ChatMessage> m_messages;
    QStringList m_activity;
    QString m_stream;
    bool m_busy = false;

    // Code snippets registered during the current render, keyed by the id used in
    // the copy:// and run:// anchors. Rebuilt on every render.
    QMap<int, QString> m_codeBlocks;
    int m_codeCounter = 0;

    // Example prompts offered on the empty state, referenced by index in prompt://
    // anchors.
    QStringList m_examples;
};
//=============================================================================
