//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtAssistantBrowser.h"
#include "AssistantSink.hpp"
#include "EvaluateCommand.hpp"
#include "Evaluator.hpp"
#include "MainThreadTaskQueue.hpp"
#include "PostCommand.hpp"
#include "QtDockTitleBar.h"
#include "QtTranslation.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QEvent>
#include <QtCore/QJsonArray>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QPointer>
#include <QtCore/QRegularExpression>
#include <QtGui/QClipboard>
#include <QtGui/QDesktopServices>
#include <QtGui/QPalette>
#include <QtGui/QTextCursor>
#include <QtWidgets/QApplication>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>
//=============================================================================
using namespace Nelson;
//=============================================================================
// Nelson single-quoted string literal escaping ('' for ', line breaks -> space).
static std::wstring
singleQuoteSafe(const QString& text)
{
    std::wstring out;
    const std::wstring w = text.toStdWString();
    out.reserve(w.size());
    for (wchar_t c : w) {
        if (c == L'\'') {
            out += L"''";
        } else if (c == L'\r' || c == L'\n') {
            out += L' ';
        } else {
            out += c;
        }
    }
    return out;
}
//=============================================================================
QtAssistantBrowser::QtAssistantBrowser(QWidget* parent) : QDockWidget(parent)
{
    setWindowTitle(TR("Assistant"));
    setObjectName("AssistantDock");
    // Same chevron title bar as the other docks (Command Window, browsers).
    setTitleBarWidget(new QtDockTitleBar(this, "ai"));

    m_examples << TR("Plot sin(x) from -pi to pi")
               << TR("Compute the sum of the integers from 1 to 100")
               << TR("Create a 3-by-3 magic square and show its row sums")
               << TR("Explain what eig does");

    QWidget* content = new QWidget(this);
    QVBoxLayout* layout = new QVBoxLayout(content);
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(6);

    // Model selector, right-aligned (the dock title bar already names the panel).
    QHBoxLayout* header = new QHBoxLayout();
    m_permMode = new QComboBox(content);
    m_permMode->setToolTip(TR("When to ask before the assistant accesses files"));
    m_permMode->addItem(TR("Always ask"), "always_ask");
    m_permMode->addItem(TR("Approve for me"), "auto");
    m_permMode->addItem(TR("Full access"), "full");
    connect(m_permMode, &QComboBox::currentIndexChanged, this, [this](int) {
        const std::wstring mode = m_permMode->currentData().toString().toStdWString();
        const std::wstring command = L"nelson.ai.acp.permissionMode('" + mode + L"');";
        Nelson::MainThreadTaskQueue::getInstance()->postToInterpreterThread(
            [command](Nelson::Evaluator* eval) {
                if (eval == nullptr) {
                    return;
                }
                std::wstring out;
                try {
                    Nelson::EvaluateConsoleCommandToString(eval, command, out);
                } catch (...) {
                }
            });
    });
    m_model = new QComboBox(content);
    m_model->addItem("Claude (ACP)", "claude-agent-acp");
    m_model->installEventFilter(this);
    header->addStretch(1);
    header->addWidget(m_permMode);
    header->addWidget(m_model);
    m_newChat = new QPushButton(TR("New"), content);
    m_newChat->setToolTip(TR("Clear the conversation and start fresh"));
    header->addWidget(m_newChat);
    layout->addLayout(header);

    m_output = new QTextBrowser(content);
    m_output->setOpenLinks(false);
    m_output->setOpenExternalLinks(false);
    layout->addWidget(m_output, 1);

    QHBoxLayout* row = new QHBoxLayout();
    m_input = new QLineEdit(content);
    m_input->setPlaceholderText(TR("Ask the assistant…"));
    m_send = new QPushButton(TR("Send"), content);
    row->addWidget(m_input);
    row->addWidget(m_send);
    layout->addLayout(row);

    setWidget(content);

    m_pollTimer = new QTimer(this);
    m_pollTimer->setInterval(150);
    connect(m_pollTimer, &QTimer::timeout, this, &QtAssistantBrowser::onPollTick);

    connect(m_send, &QPushButton::clicked, this, &QtAssistantBrowser::onSend);
    connect(m_newChat, &QPushButton::clicked, this, &QtAssistantBrowser::onNewConversation);
    connect(m_input, &QLineEdit::returnPressed, this, &QtAssistantBrowser::onSend);
    // Re-read the shared conversation whenever the dock is shown/fronted, so it
    // catches up with a talk continued in another GUI (seeds only if empty+idle).
    connect(this, &QDockWidget::visibilityChanged, this, [this](bool visible) {
        if (visible) {
            refreshHistory();
        }
    });
    connect(m_output, &QTextBrowser::anchorClicked, this, &QtAssistantBrowser::onAnchorClicked);
    connect(this, &QtAssistantBrowser::assistantEvent, this, &QtAssistantBrowser::onAssistantEvent,
        Qt::QueuedConnection);

    // The sink is called from the interpreter thread: emit a queued signal so the
    // text is updated on the GUI thread only.
    QPointer<QtAssistantBrowser> self(this);
    Nelson::RegisterAssistantSink(
        "qt", [self](const std::string& channel, const std::string& payload) {
            if (self) {
                emit self->assistantEvent(
                    QString::fromStdString(channel), QString::fromStdString(payload));
            }
        });

    rebuild();
    requestModels();
    // Restore the prior conversation of this surface so a restart re-displays it.
    requestHistory();
    // Retry the model list once the interpreter has settled (a local provider may
    // not be reachable at the first query).
    QTimer::singleShot(2000, this, [this]() { requestModels(); });
    // Keep re-reading the shared history at short intervals until the dock is seeded:
    // the startup read can run before the engine is ready and return empty. Stops as
    // soon as a turn appears, or after a few seconds if the conversation is empty.
    m_historyRetryTimer = new QTimer(this);
    m_historyRetryTimer->setInterval(250);
    connect(m_historyRetryTimer, &QTimer::timeout, this, [this]() {
        if (!m_messages.isEmpty() || m_historyRetryAttempts >= 16) {
            m_historyRetryTimer->stop();
            return;
        }
        ++m_historyRetryAttempts;
        requestHistory();
    });
    m_historyRetryTimer->start();
}
//=============================================================================
bool
QtAssistantBrowser::eventFilter(QObject* watched, QEvent* event)
{
    if (watched == m_model && event->type() == QEvent::MouseButtonPress) {
        requestModels();
    }
    return QDockWidget::eventFilter(watched, event);
}
//=============================================================================
QtAssistantBrowser::~QtAssistantBrowser() { Nelson::UnregisterAssistantSink("qt"); }
//=============================================================================
void
QtAssistantBrowser::refreshHistory()
{
    requestHistory();
}
//=============================================================================
void
QtAssistantBrowser::onSend()
{
    if (m_busy) {
        // Acts as Stop while a request is running: the "qt" surface poll loop
        // checks this per-surface flag and aborts cleanly, without touching the
        // web pane or the NFlow pane.
        Nelson::RequestAssistantStop("qt");
        return;
    }
    const QString text = m_input->text().trimmed();
    if (text.isEmpty()) {
        return;
    }
    m_input->clear();
    submitPrompt(text);
}
//=============================================================================
void
QtAssistantBrowser::onNewConversation()
{
    // Stop any running turn, clear the on-screen transcript now, then on the
    // interpreter thread forget the shared stored history and reset the live agent,
    // so the next prompt starts clean. The Qt dock and the web pane share one
    // conversation, so this clears both.
    if (m_busy) {
        Nelson::RequestAssistantStop("qt");
    }
    m_busy = false;
    m_messages.clear();
    m_activity.clear();
    m_stream.clear();
    rebuild();
    Nelson::MainThreadTaskQueue::getInstance()->postToInterpreterThread([](Nelson::Evaluator* eval) {
        if (eval == nullptr) {
            return;
        }
        std::wstring out;
        try {
            Nelson::EvaluateConsoleCommandToString(
                eval, L"try; nelson.ai.internal.newConversation('qt'); catch; end", out);
        } catch (...) {
        }
    });
}
//=============================================================================
void
QtAssistantBrowser::submitPrompt(const QString& prompt)
{
    if (m_busy || prompt.trimmed().isEmpty()) {
        return;
    }
    m_messages.append(ChatMessage { QStringLiteral("user"), prompt });
    m_activity.clear();
    m_stream.clear();
    m_busy = true;
    m_send->setText(TR("Stop"));
    rebuild();

    const QString model = m_model->currentData().toString();
    // 'external' driver (this dock drives the poll from its own QTimer below) and
    // surfaceId 'qt' so this dock owns an isolated ACP session.
    const std::wstring command = L"nelson.ai.internal.assistantRun('" + singleQuoteSafe(prompt) + L"', '"
        + singleQuoteSafe(model) + L"', 'external', 'qt');";
    postCommand(command);
    m_pollTimer->start();
}
//=============================================================================
// Advance the claude-agent-acp reply one step on the interpreter thread; the
// streamed tokens and final answer come back as assistant events.
void
QtAssistantBrowser::onPollTick()
{
    if (!m_busy) {
        m_pollTimer->stop();
        return;
    }
    Nelson::MainThreadTaskQueue::getInstance()->postToInterpreterThread(
        [](Nelson::Evaluator* eval) {
            if (eval == nullptr) {
                return;
            }
            std::wstring out;
            try {
                Nelson::EvaluateConsoleCommandToString(
                    eval, L"nelson.ai.internal.assistantStep('qt');", out);
            } catch (...) {
            }
        });
}
//=============================================================================
void
QtAssistantBrowser::onAssistantEvent(const QString& channel, const QString& payload)
{
    const QJsonObject object = QJsonDocument::fromJson(payload.toUtf8()).object();
    if (channel == "showPanel") {
        // A host action (for example profile_current_file) asks to bring a GUI panel
        // to the front; forward it to the main window, which owns the docks.
        const QString panel = object.value("panel").toString();
        if (!panel.isEmpty()) {
            emit requestShowPanel(panel);
        }
        return;
    }
    if (channel == "models") {
        const QString current = m_model->currentData().toString();
        m_model->clear();
        m_model->addItem("Claude (ACP)", "claude-agent-acp");
        const QJsonArray models = object.value("models").toArray();
        for (const QJsonValue& value : models) {
            const QString name = value.toString();
            if (!name.isEmpty()) {
                m_model->addItem(name, name);
            }
        }
        const int index
            = m_model->findData(current.isEmpty() ? QStringLiteral("claude-agent-acp") : current);
        if (index >= 0) {
            m_model->setCurrentIndex(index);
        }
        return;
    }
    if (channel == "history") {
        // Seed the prior conversation only while the pane is still empty and idle,
        // so a turn the user started before the history arrived is never clobbered.
        if (!m_messages.isEmpty() || m_busy) {
            return;
        }
        const QJsonArray turns = object.value("turns").toArray();
        for (const QJsonValue& value : turns) {
            const QJsonObject turn = value.toObject();
            const QString role = turn.value("role").toString();
            const QString text = turn.value("text").toString();
            if ((role == "user" || role == "assistant") && !text.isEmpty()) {
                m_messages.append(ChatMessage { role, text });
            }
        }
        if (!m_messages.isEmpty()) {
            rebuild();
        }
        return;
    }
    if (channel == "reset") {
        m_stream.clear();
        rebuild();
        return;
    }
    if (channel == "token") {
        m_stream += object.value("text").toString();
        rebuild();
        return;
    }
    if (channel == "activity") {
        const QString kind = object.value("kind").toString();
        const QString name = object.value("name").toString();
        const QString text = object.value("text").toString();
        QString line;
        if (kind == "result") {
            // Built from code points (U+2713 check, U+25B6 play): a raw UTF-8 glyph
            // literal mojibakes when MSVC compiles this source under the system code page.
            line = QString(QChar(0x2713)) + QLatin1Char(' ') + name
                + (text.isEmpty() ? QString() : (": " + text));
        } else {
            line = QString(QChar(0x25B6)) + QLatin1Char(' ')
                + (name.isEmpty() ? TR("working") : name);
        }
        m_activity.append(line);
        m_stream.clear();
        rebuild();
        return;
    }
    if (channel == "permission") {
        // The agent asks to use a file tool: let the user decide (Claude Code
        // style), then send the answer back on the 'qt' surface. Default is reject,
        // matching the engine's safe fallback, so a dismissed dialog never grants.
        const long long id = static_cast<long long>(object.value("id").toDouble());
        const QString title = object.value("title").toString();
        QMessageBox box(this);
        box.setIcon(QMessageBox::Question);
        box.setWindowTitle(TR("Assistant permission"));
        box.setText(title.isEmpty() ? TR("The assistant requests file access.") : title);
        QPushButton* allowOnce = box.addButton(TR("Allow once"), QMessageBox::AcceptRole);
        // "Always for this file" remembers just this path for the session; a global
        // "never ask" is the Full access permission mode, so no global allow here.
        QPushButton* allowAlways
            = box.addButton(TR("Always for this file"), QMessageBox::AcceptRole);
        QPushButton* rejectButton = box.addButton(TR("Reject"), QMessageBox::RejectRole);
        box.setDefaultButton(rejectButton);
        box.exec();
        std::wstring decision = L"reject";
        if (box.clickedButton() == allowOnce) {
            decision = L"allow_once";
        } else if (box.clickedButton() == allowAlways) {
            decision = L"allow_always_path";
        }
        const std::wstring command = L"nelson.ai.internal.assistantPermissionReply('qt', "
            + std::to_wstring(id) + L", '" + decision + L"');";
        Nelson::MainThreadTaskQueue::getInstance()->postToInterpreterThread(
            [command](Nelson::Evaluator* eval) {
                if (eval == nullptr) {
                    return;
                }
                std::wstring out;
                try {
                    Nelson::EvaluateConsoleCommandToString(eval, command, out);
                } catch (...) {
                }
            });
        return;
    }
    if (channel == "answer" || channel == "error") {
        QString text = object.value("text").toString();
        if (text.isEmpty()) {
            text = (channel == "answer") ? TR("(no response)") : TR("The assistant failed.");
        }
        m_messages.append(ChatMessage { QStringLiteral("assistant"), text });
        m_busy = false;
        m_send->setText(TR("Send"));
        m_stream.clear();
        m_activity.clear();
        m_pollTimer->stop();
        rebuild();
    }
}
//=============================================================================
void
QtAssistantBrowser::onAnchorClicked(const QUrl& url)
{
    const QString scheme = url.scheme();
    if (scheme == "copy" || scheme == "run") {
        // The id is carried in the path, not a numeric authority: Qt's QUrl parses
        // "copy://5" as an IPv4 host ("0.0.0.5"), so host().toInt() is always 0.
        const int id = url.path().toInt();
        const QString code = m_codeBlocks.value(id);
        if (code.isEmpty()) {
            return;
        }
        if (scheme == "copy") {
            QApplication::clipboard()->setText(code);
        } else {
            postCommand(code.toStdWString());
        }
        return;
    }
    if (scheme == "prompt") {
        // Index is in the path, not a numeric authority (QUrl would read it as an IP).
        const int index = url.path().toInt();
        if (index >= 0 && index < m_examples.size()) {
            submitPrompt(m_examples.at(index));
        }
        return;
    }
    if (scheme == "http" || scheme == "https") {
        QDesktopServices::openUrl(url);
    }
}
//=============================================================================
void
QtAssistantBrowser::changeEvent(QEvent* event)
{
    QDockWidget::changeEvent(event);
    if (event != nullptr
        && (event->type() == QEvent::PaletteChange || event->type() == QEvent::StyleChange)) {
        rebuild();
    }
}
//=============================================================================
// Blend two colours (t=0 -> a, t=1 -> b).
static QString
blendName(const QColor& a, const QColor& b, double t)
{
    return QColor(static_cast<int>(a.red() + (b.red() - a.red()) * t),
        static_cast<int>(a.green() + (b.green() - a.green()) * t),
        static_cast<int>(a.blue() + (b.blue() - a.blue()) * t))
        .name();
}
//=============================================================================
// Derive the panel colours from only Base / Text (the Command Window's own
// background / foreground) so the panel reads like the terminal in any theme.
// Deriving shades by blending those two is theme-safe, unlike palette roles such
// as AlternateBase (can be near-black) or Highlight (a loud accent), which made
// the bubbles unreadable.
void
QtAssistantBrowser::updateTheme()
{
    const QPalette pal = m_output->palette();
    const QColor base = pal.color(QPalette::Base);
    const QColor text = pal.color(QPalette::Text);
    m_theme.text = text.name();
    m_theme.userText = text.name();
    m_theme.cardBg = base.name(); // assistant stays flat on the terminal background
    m_theme.muted = blendName(base, text, 0.45);
    m_theme.border = blendName(base, text, 0.22);
    m_theme.codeBg = blendName(base, text, 0.06);
    m_theme.userBg = blendName(base, text, 0.09); // subtle tint, not a loud fill
    m_theme.link = pal.color(QPalette::Highlight).name();
}
//=============================================================================
// Enumerate the local provider models (Ollama / openai-compatible) off the GUI
// thread and feed them to the selector, mirroring the web pane. The agent query
// hits the network, so it runs on the interpreter thread and reports back through
// the sink as a 'models' event; failures fall back to the ACP entry alone.
void
QtAssistantBrowser::requestModels()
{
    Nelson::MainThreadTaskQueue::getInstance()->postToInterpreterThread(
        [](Nelson::Evaluator* eval) {
            if (eval == nullptr) {
                return;
            }
            std::wstring out;
            try {
                Nelson::EvaluateConsoleCommandToString(eval,
                    L"try; disp(jsonencode(nelson.ai.models())); catch; disp('[]'); end", out);
            } catch (...) {
                out.clear();
            }
            std::string list = Nelson::wstring_to_utf8(out);
            const size_t start = list.find_first_not_of(" \t\r\n");
            const size_t end = list.find_last_not_of(" \t\r\n");
            if (start == std::string::npos || end == std::string::npos) {
                list = "[]";
            } else {
                list = list.substr(start, end - start + 1);
            }
            if (list.empty() || list.front() != '[') {
                list = "[]";
            }
            // Route to THIS dock's registered "qt" sink. The legacy no-arg
            // AssistantSinkPush targets a global sink nothing ever sets (a dead
            // no-op), so the "models" event never reached onAssistantEvent and the
            // local providers never populated the selector.
            Nelson::AssistantSinkPushTo("qt", "models", std::string("{\"models\":") + list + "}");
        });
}
//=============================================================================
// Load this dock's stored conversation off the GUI thread and feed it back as a
// 'history' event, so a restart re-displays the prior turns. The JSON is built on
// the interpreter thread (where the history package lives) and pushed to the 'qt'
// sink only; onAssistantEvent seeds m_messages from it while the pane is empty.
void
QtAssistantBrowser::requestHistory()
{
    Nelson::MainThreadTaskQueue::getInstance()->postToInterpreterThread(
        [](Nelson::Evaluator* eval) {
            if (eval == nullptr) {
                return;
            }
            std::wstring out;
            try {
                Nelson::EvaluateConsoleCommandToString(
                    eval, L"try; disp(nelson.ai.internal.history.json('qt')); catch; disp('[]'); end", out);
            } catch (...) {
                out.clear();
            }
            std::string json = Nelson::wstring_to_utf8(out);
            const size_t start = json.find_first_not_of(" \t\r\n");
            const size_t end = json.find_last_not_of(" \t\r\n");
            if (start == std::string::npos || end == std::string::npos) {
                json = "[]";
            } else {
                json = json.substr(start, end - start + 1);
            }
            if (json.empty() || json.front() != '[') {
                json = "[]";
            }
            Nelson::AssistantSinkPushTo("qt", "history", std::string("{\"turns\":") + json + "}");
        });
}
//=============================================================================
void
QtAssistantBrowser::rebuild()
{
    updateTheme();
    m_codeBlocks.clear();
    m_codeCounter = 0;
    m_output->setHtml(renderMessages());
    m_output->moveCursor(QTextCursor::End);
    m_output->ensureCursorVisible();
}
//=============================================================================
QString
QtAssistantBrowser::renderMessages()
{
    QString html = "<div style=\"font-size:13px; color:" + m_theme.text + ";\">";

    if (m_messages.isEmpty() && !m_busy) {
        html += "<div style=\"color:" + m_theme.muted + "; margin:8px 2px;\">"
            + escapeHtml(TR("Ask the assistant to write or run Nelson code.")) + "</div>";
        for (int i = 0; i < m_examples.size(); ++i) {
            html += "<div style=\"margin:4px 2px;\"><a href=\"prompt:" + QString::number(i)
                + "\" style=\"color:" + m_theme.link + "; text-decoration:none;\">"
                + QString(QChar(0x2022)) + " " + escapeHtml(m_examples.at(i)) + "</a></div>";
        }
    }

    for (const ChatMessage& message : m_messages) {
        const bool isUser = (message.role == "user");
        const QString roleLabel = isUser ? TR("You") : TR("Assistant");
        html += "<div style=\"color:" + m_theme.muted
            + "; font-size:11px; margin:10px 2px 2px 2px;\">" + escapeHtml(roleLabel) + "</div>";
        if (isUser) {
            // A faint tinted strip keeps the prompt readable without a loud fill.
            html += "<table width=\"100%\" cellspacing=\"0\" cellpadding=\"0\"><tr>"
                    "<td style=\"background:"
                + m_theme.userBg + "; padding:6px 8px; color:" + m_theme.userText + ";\">"
                + renderBubble(message.text) + "</td></tr></table>";
        } else {
            // The reply stays flat on the terminal background, like the console.
            html += "<div style=\"margin:0 2px 2px 2px; color:" + m_theme.text + ";\">"
                + renderBubble(message.text) + "</div>";
        }
    }

    if (m_busy) {
        html += "<div style=\"color:" + m_theme.muted + "; font-size:12px; margin:8px 2px;\">"
            + escapeHtml(TR("Thinking…")) + "</div>";
        for (const QString& line : m_activity) {
            html += "<div style=\"color:" + m_theme.muted + "; font-size:12px; margin:2px 2px;\">"
                + escapeHtml(line) + "</div>";
        }
        const QString trimmed = m_stream.trimmed();
        if (!trimmed.isEmpty() && !trimmed.startsWith('{')) {
            html += "<div style=\"color:" + m_theme.text
                + "; margin:4px 2px; "
                  "white-space:pre-wrap;\">"
                + renderInline(m_stream) + "</div>";
        }
    }

    html += "</div>";
    return html;
}
//=============================================================================
// Splits the reply into text and fenced-code segments (same shape as the web
// pane) and renders each: text through a light inline markdown, code as a box
// with Copy / Run actions.
QString
QtAssistantBrowser::renderBubble(const QString& text)
{
    QString html;
    QRegularExpression fence("```[\\w-]*\\r?\\n?([\\s\\S]*?)```");
    int last = 0;
    QRegularExpressionMatchIterator it = fence.globalMatch(text);
    while (it.hasNext()) {
        const QRegularExpressionMatch match = it.next();
        if (match.capturedStart() > last) {
            html += renderInline(text.mid(last, match.capturedStart() - last));
        }
        QString code = match.captured(1);
        code.remove(QRegularExpression("\\r?\\n$"));
        const int id = m_codeCounter++;
        m_codeBlocks.insert(id, code);
        html += "<table width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" "
                "style=\"margin:6px 0;\"><tr>"
                "<td style=\"background:"
            + m_theme.codeBg + "; border:1px solid " + m_theme.border
            + ";\"><div style=\"text-align:right; padding:2px 6px;\">"
              "<a href=\"copy:"
            + QString::number(id) + "\" style=\"color:" + m_theme.link
            + "; text-decoration:none;\">" + escapeHtml(TR("Copy"))
            + "</a>&nbsp;&nbsp;<a href=\"run:" + QString::number(id) + "\" style=\"color:"
            + m_theme.link + "; text-decoration:none;\">" + escapeHtml(TR("Run"))
            + "</a></div>"
              "<pre style=\"margin:0; padding:8px; color:"
            + m_theme.text + "; font-family:Consolas,'Courier New',monospace;\">" + escapeHtml(code)
            + "</pre></td></tr></table>";
        last = match.capturedEnd();
    }
    if (last < text.size()) {
        html += renderInline(text.mid(last));
    }
    if (html.isEmpty()) {
        html = renderInline(text);
    }
    return html;
}
//=============================================================================
// Minimal inline markdown: **bold**, `code`, line breaks. Everything is HTML
// escaped first so model output can never inject markup.
QString
QtAssistantBrowser::renderInline(const QString& text) const
{
    QString html = escapeHtml(text);
    QRegularExpression bold("\\*\\*(.+?)\\*\\*");
    html.replace(bold, "<b>\\1</b>");
    QRegularExpression code("`([^`]+?)`");
    html.replace(code,
        "<code style=\"background:" + m_theme.codeBg
            + "; padding:0 3px; font-family:Consolas,'Courier New',monospace;\">\\1</code>");
    html.replace("\n", "<br>");
    return html;
}
//=============================================================================
QString
QtAssistantBrowser::escapeHtml(const QString& text)
{
    QString out = text;
    out.replace('&', "&amp;");
    out.replace('<', "&lt;");
    out.replace('>', "&gt;");
    return out;
}
//=============================================================================
