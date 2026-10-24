//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QUrl>
#include <QtCore/QXmlStreamReader>
#include <QtGui/QDesktopServices>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QScrollBar>
#include <QtNetwork/QNetworkRequest>
#include "QtNewsfeedBrowser.h"
#include "QtDockTitleBar.h"
#include "QtTranslation.hpp"
#include "NelsonConfiguration.hpp"
#include "QStringConverter.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// A few feeds offered by default in the address combo.
static const char* DEFAULT_FEED = "https://github.com/nelson-lang/nelson/releases.atom";
//=============================================================================
static QString
resourcePath(const QString& name)
{
    return wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory())
        + QString("/resources/") + name;
}
//=============================================================================
static QString
escapeHtml(const QString& text)
{
    QString out(text);
    out.replace('&', "&amp;");
    out.replace('<', "&lt;");
    out.replace('>', "&gt;");
    out.replace('"', "&quot;");
    return out;
}
//=============================================================================
NewsfeedView::NewsfeedView(QWidget* parent) : QTextBrowser(parent) { }
//=============================================================================
QVariant
NewsfeedView::loadResource(int type, const QUrl& name)
{
    if (type == static_cast<int>(QTextDocument::ImageResource)) {
        auto it = imageCache.find(name.toString());
        if (it != imageCache.end()) {
            return *it;
        }
        return QVariant();
    }
    return QTextBrowser::loadResource(type, name);
}
//=============================================================================
QtNewsfeedBrowser::QtNewsfeedBrowser(QWidget* parent)
    : QDockWidget(TR("News feed"), parent)
    , m_toolBar(nullptr)
    , m_refreshAction(nullptr)
    , m_urlEdit(nullptr)
    , m_view(nullptr)
    , m_status(nullptr)
    , m_net(nullptr)
    , m_imageNet(nullptr)
    , m_feedTitle()
    , m_feedLink()
    , m_pendingImages(0)
    , m_firstShow(true)
{
    setObjectName("newsfeedBrowser");
    m_net = new QNetworkAccessManager(this);
    m_imageNet = new QNetworkAccessManager(this);
    connect(m_net, &QNetworkAccessManager::finished, this, &QtNewsfeedBrowser::onFeedFinished);
    connect(
        m_imageNet, &QNetworkAccessManager::finished, this, &QtNewsfeedBrowser::onImageFinished);
    setupUi();
    setTitleBarWidget(new QtDockTitleBar(this, "nf"));
    hide();
}
//=============================================================================
void
QtNewsfeedBrowser::setupUi()
{
    QWidget* centralWidget = new QWidget(this);
    QVBoxLayout* layout = new QVBoxLayout(centralWidget);
    layout->setContentsMargins(4, 4, 4, 4);
    layout->setSpacing(4);

    m_toolBar = new QToolBar(TR("News feed Toolbar"), centralWidget);
    m_toolBar->setObjectName("NewsfeedToolBar");
    m_toolBar->setIconSize(QSize(18, 18));
    m_toolBar->setMovable(false);
    m_toolBar->setFloatable(false);

    m_urlEdit = new QComboBox(m_toolBar);
    m_urlEdit->setEditable(true);
    m_urlEdit->setInsertPolicy(QComboBox::NoInsert);
    m_urlEdit->setMinimumWidth(220);
    m_urlEdit->addItem(QString::fromLatin1(DEFAULT_FEED));
    m_urlEdit->addItem("https://hnrss.org/frontpage");
    m_urlEdit->addItem("https://feeds.bbci.co.uk/news/rss.xml");
    m_urlEdit->setCurrentIndex(0);
    connect(m_urlEdit->lineEdit(), &QLineEdit::returnPressed, this, &QtNewsfeedBrowser::refresh);
    m_toolBar->addWidget(m_urlEdit);

    m_refreshAction = new QAction(QIcon(resourcePath("refresh.svg")), TR("Refresh"), this);
    m_refreshAction->setShortcut(QKeySequence::Refresh);
    m_refreshAction->setToolTip(TR("Reload the feed"));
    connect(m_refreshAction, &QAction::triggered, this, &QtNewsfeedBrowser::refresh);
    m_toolBar->addAction(m_refreshAction);

    layout->addWidget(m_toolBar);

    m_view = new NewsfeedView(centralWidget);
    m_view->setOpenExternalLinks(true);
    m_view->setOpenLinks(true);
    layout->addWidget(m_view, 1);

    m_status = new QLabel(centralWidget);
    m_status->setObjectName("NewsfeedStatusLabel");
    layout->addWidget(m_status);

    setWidget(centralWidget);
}
//=============================================================================
void
QtNewsfeedBrowser::loadFeed(const QString& url)
{
    if (!url.isEmpty()) {
        int index = m_urlEdit->findText(url);
        if (index >= 0) {
            m_urlEdit->setCurrentIndex(index);
        } else {
            m_urlEdit->setEditText(url);
        }
    }
    refresh();
}
//=============================================================================
void
QtNewsfeedBrowser::refresh()
{
    QString url = m_urlEdit->currentText().trimmed();
    if (url.isEmpty()) {
        return;
    }
    setStatus(TR("Loading..."));
    QNetworkRequest request((QUrl(url)));
    request.setHeader(QNetworkRequest::UserAgentHeader, QByteArray("Nelson newsfeed/1.0"));
    request.setAttribute(
        QNetworkRequest::RedirectPolicyAttribute, QNetworkRequest::NoLessSafeRedirectPolicy);
    m_net->get(request);
}
//=============================================================================
void
QtNewsfeedBrowser::onFeedFinished(QNetworkReply* reply)
{
    reply->deleteLater();
    if (reply->error() != QNetworkReply::NoError) {
        setStatus(TR("Unable to load the feed: ") + reply->errorString());
        return;
    }
    parseFeed(reply->readAll());
    renderItems();
}
//=============================================================================
void
QtNewsfeedBrowser::parseFeed(const QByteArray& data)
{
    m_items.clear();
    m_feedTitle.clear();
    m_feedLink.clear();
    m_view->imageCache.clear();

    QXmlStreamReader xml(data);
    NewsfeedItem cur;
    bool inItem = false;
    while (!xml.atEnd()) {
        QXmlStreamReader::TokenType token = xml.readNext();
        if (token == QXmlStreamReader::StartElement) {
            const QString name = xml.name().toString();
            if (name == "item" || name == "entry") {
                inItem = true;
                cur = NewsfeedItem();
            } else if (inItem) {
                if (name == "title") {
                    cur.title
                        = xml.readElementText(QXmlStreamReader::IncludeChildElements).trimmed();
                } else if (name == "link") {
                    const QString href = xml.attributes().value("href").toString();
                    if (!href.isEmpty()) {
                        const QString rel = xml.attributes().value("rel").toString();
                        if (cur.link.isEmpty() || rel == "alternate") {
                            cur.link = href;
                        }
                    } else {
                        cur.link = xml.readElementText().trimmed();
                    }
                } else if (name == "pubDate" || name == "updated" || name == "published"
                    || name == "date") {
                    if (cur.date.isEmpty()) {
                        cur.date = xml.readElementText().trimmed();
                    }
                } else if (name == "thumbnail"
                    || (name == "content" && xml.attributes().hasAttribute("url"))) {
                    const QString u = xml.attributes().value("url").toString();
                    if (!u.isEmpty() && cur.image.isEmpty()) {
                        cur.image = u;
                    }
                } else if (name == "enclosure") {
                    const QString type = xml.attributes().value("type").toString();
                    const QString u = xml.attributes().value("url").toString();
                    if (!u.isEmpty() && cur.image.isEmpty()
                        && (type.isEmpty() || type.startsWith("image"))) {
                        cur.image = u;
                    }
                } else if (name == "description" || name == "summary" || name == "content"
                    || name == "encoded") {
                    if (cur.summary.isEmpty()) {
                        cur.summary
                            = xml.readElementText(QXmlStreamReader::IncludeChildElements).trimmed();
                    }
                }
            } else {
                if (name == "title" && m_feedTitle.isEmpty()) {
                    m_feedTitle = xml.readElementText().trimmed();
                } else if (name == "link" && m_feedLink.isEmpty()) {
                    const QString href = xml.attributes().value("href").toString();
                    m_feedLink = href.isEmpty() ? xml.readElementText().trimmed() : href;
                }
            }
        } else if (token == QXmlStreamReader::EndElement) {
            const QString name = xml.name().toString();
            if (name == "item" || name == "entry") {
                inItem = false;
                if (m_items.size() < 50) {
                    m_items.append(cur);
                }
            }
        }
    }
}
//=============================================================================
void
QtNewsfeedBrowser::renderItems()
{
    m_view->setHtml(buildHtml());
    m_pendingImages = 0;
    for (const NewsfeedItem& item : m_items) {
        if (item.image.isEmpty() || item.image.startsWith("data:")) {
            continue;
        }
        if (m_view->imageCache.contains(item.image)) {
            continue;
        }
        m_pendingImages++;
        QNetworkRequest request((QUrl(item.image)));
        request.setAttribute(
            QNetworkRequest::RedirectPolicyAttribute, QNetworkRequest::NoLessSafeRedirectPolicy);
        m_imageNet->get(request);
    }
    if (m_items.isEmpty()) {
        setStatus(TR("No item in this feed."));
    } else {
        setStatus(QString::number(m_items.size()) + " " + TR("items"));
    }
}
//=============================================================================
void
QtNewsfeedBrowser::onImageFinished(QNetworkReply* reply)
{
    reply->deleteLater();
    if (m_pendingImages > 0) {
        m_pendingImages--;
    }
    if (reply->error() == QNetworkReply::NoError) {
        QImage image;
        if (image.loadFromData(reply->readAll())) {
            m_view->imageCache.insert(reply->url().toString(), image);
        }
    }
    if (m_pendingImages == 0) {
        // Re-render once every thumbnail is cached so loadResource resolves them.
        const int scroll = m_view->verticalScrollBar() ? m_view->verticalScrollBar()->value() : 0;
        m_view->setHtml(buildHtml());
        if (m_view->verticalScrollBar()) {
            m_view->verticalScrollBar()->setValue(scroll);
        }
    }
}
//=============================================================================
QString
QtNewsfeedBrowser::buildHtml() const
{
    QString html;
    html += "<html><body>";
    if (!m_feedTitle.isEmpty()) {
        html += "<h2>" + escapeHtml(m_feedTitle) + "</h2>";
    }
    for (const NewsfeedItem& item : m_items) {
        html += "<table width=\"100%\" cellspacing=\"0\" cellpadding=\"4\"><tr>";
        if (!item.image.isEmpty()) {
            html += "<td width=\"120\" valign=\"top\"><img src=\"" + escapeHtml(item.image)
                + "\" width=\"112\"></td>";
        }
        html += "<td valign=\"top\">";
        QString title
            = escapeHtml(item.title.isEmpty() ? QStringLiteral("(no title)") : item.title);
        if (!item.link.isEmpty()) {
            html += "<p><a href=\"" + escapeHtml(item.link) + "\"><b>" + title + "</b></a></p>";
        } else {
            html += "<p><b>" + title + "</b></p>";
        }
        if (!item.date.isEmpty()) {
            html += "<p><font color=\"gray\" size=\"-1\">" + escapeHtml(item.date) + "</font></p>";
        }
        if (!item.summary.isEmpty()) {
            html += "<div>" + item.summary + "</div>";
        }
        html += "</td></tr></table><hr>";
    }
    html += "</body></html>";
    return html;
}
//=============================================================================
void
QtNewsfeedBrowser::setStatus(const QString& message)
{
    if (m_status) {
        m_status->setText(message);
    }
}
//=============================================================================
void
QtNewsfeedBrowser::closeEvent(QCloseEvent* event)
{
    emit closeNewsfeedBrowser();
    QDockWidget::closeEvent(event);
}
//=============================================================================
void
QtNewsfeedBrowser::showEvent(QShowEvent* event)
{
    QDockWidget::showEvent(event);
    // Fetch the selected feed the first time the panel becomes visible, so it
    // is never shown empty; no network request is issued until then.
    if (m_firstShow) {
        m_firstShow = false;
        refresh();
    }
}
//=============================================================================
