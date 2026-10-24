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
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtWidgets/QWidget>
//=============================================================================
class QComboBox;
class QLineEdit;
class QToolButton;
//=============================================================================
class CurrentDirectoryWidget : public QWidget
{
    Q_OBJECT

public:
    explicit CurrentDirectoryWidget(QWidget* parent = nullptr);

    void
    setDirectory(const QString& directory);

    void
    setDirectories(const QStringList& directories);

    static bool
    isValidDirectory(const QString& directory);

signals:
    void
    directoryChangeRequested(const QString& directory);
    void
    browseRequested();

private slots:
    void
    acceptDirectoryText();

    void
    activateDirectory(int index);

private:
    QComboBox* m_comboBox;
    QLineEdit* m_lineEdit;
    QToolButton* m_browseButton;
};
//=============================================================================
