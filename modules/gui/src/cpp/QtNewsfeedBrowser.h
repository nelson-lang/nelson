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
#include <QtCore/QtGlobal>
#include <QtCore/QList>
#include <QtCore/QHash>
#include <QtGui/QAction>
#include <QtGui/QImage>
#include <QtGui/QCloseEvent>
#include <QtGui/QShowEvent>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QTextBrowser>
#include <QtWidgets/QToolBar>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkReply>
//=============================================================================
// One parsed feed entry, format-agnostic (RSS 2.0 or Atom).
struct NewsfeedItem
{
    QString title;
    QString link;
    QString date;
    QString summary;
    QString image;
};
//=============================================================================
// QTextBrowser that resolves remote images from a cache filled asynchronously.
class NewsfeedView : public QTextBrowser
{
    Q_OBJECT
public:
    explicit NewsfeedView(QWidget* parent = nullptr);
    QHash<QString, QImage> imageCache;

protected:
    QVariant
    loadResource(int type, const QUrl& name) override;
};
//=============================================================================
class QtNewsfeedBrowser : public QDockWidget
{
    Q_OBJECT
public:
    explicit QtNewsfeedBrowser(QWidget* parent = nullptr);

    void
    loadFeed(const QString& url);

signals:
    void
    closeNewsfeedBrowser();

protected:
    void
    closeEvent(QCloseEvent* event) override;
    void
    showEvent(QShowEvent* event) override;

private slots:
    void
    refresh();
    void
    onFeedFinished(QNetworkReply* reply);
    void
    onImageFinished(QNetworkReply* reply);

private:
    void
    setupUi();
    void
    parseFeed(const QByteArray& data);
    void
    renderItems();
    QString
    buildHtml() const;
    void
    setStatus(const QString& message);

    QToolBar* m_toolBar;
    QAction* m_refreshAction;
    QComboBox* m_urlEdit;
    NewsfeedView* m_view;
    QLabel* m_status;

    QNetworkAccessManager* m_net;
    QNetworkAccessManager* m_imageNet;
    QList<NewsfeedItem> m_items;
    QString m_feedTitle;
    QString m_feedLink;
    int m_pendingImages;
    bool m_firstShow;
};
//=============================================================================
