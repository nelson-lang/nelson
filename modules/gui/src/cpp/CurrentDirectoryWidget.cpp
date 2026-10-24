//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CurrentDirectoryWidget.h"
#include "QtTranslation.hpp"
#include <QtCore/QDir>
#include <QtCore/QSignalBlocker>
#include <QtCore/QSize>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QStyle>
#include <QtWidgets/QToolButton>
//=============================================================================
using namespace Nelson;
//=============================================================================
CurrentDirectoryWidget::CurrentDirectoryWidget(QWidget* parent) : QWidget(parent)
{
    setObjectName("NelsonDirectoryWidget");
    setAccessibleName(TR("Current directory"));
    setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    QHBoxLayout* layout = new QHBoxLayout(this);
    layout->setContentsMargins(2, 0, 2, 0);
    layout->setSpacing(3);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    m_browseButton = new QToolButton(this);
    m_browseButton->setObjectName("NelsonDirectoryBrowseButton");
    m_browseButton->setAutoRaise(true);
    m_browseButton->setIcon(style()->standardIcon(QStyle::SP_DirOpenIcon));
    m_browseButton->setMinimumSize(QSize(32, 32));
    m_browseButton->setToolTip(TR("Choose current directory"));
    m_browseButton->setAccessibleName(TR("Choose current directory"));
    connect(m_browseButton, &QToolButton::clicked, this, &CurrentDirectoryWidget::browseRequested);
    layout->addWidget(m_browseButton);

    m_comboBox = new QComboBox(this);
    m_comboBox->setObjectName("NelsonDirectoryCombo");
    m_comboBox->setAccessibleName(TR("Current directory"));
    m_comboBox->setEditable(true);
    m_comboBox->setInsertPolicy(QComboBox::NoInsert);
    m_comboBox->setMinimumWidth(260);
    m_comboBox->setMaximumWidth(320);
    m_comboBox->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    connect(m_comboBox, QOverload<int>::of(&QComboBox::activated), this,
        &CurrentDirectoryWidget::activateDirectory);

    m_lineEdit = m_comboBox->lineEdit();
    m_lineEdit->setObjectName("NelsonDirectoryField");
    m_lineEdit->setAccessibleName(TR("Current directory"));
    m_lineEdit->setClearButtonEnabled(false);
    connect(
        m_lineEdit, &QLineEdit::returnPressed, this, &CurrentDirectoryWidget::acceptDirectoryText);
    layout->addWidget(m_comboBox);
}
//=============================================================================
void
CurrentDirectoryWidget::acceptDirectoryText()
{
    const QString directory = QDir::cleanPath(m_lineEdit->text().trimmed());
    if (isValidDirectory(directory)) {
        m_lineEdit->setText(QDir::toNativeSeparators(directory));
        emit directoryChangeRequested(directory);
    } else {
        m_lineEdit->setText(QDir::toNativeSeparators(QDir::currentPath()));
    }
}
//=============================================================================
void
CurrentDirectoryWidget::activateDirectory(int index)
{
    const QString directory = m_comboBox->itemData(index).toString();
    if (isValidDirectory(directory)) {
        emit directoryChangeRequested(directory);
    }
}
//=============================================================================
void
CurrentDirectoryWidget::setDirectory(const QString& directory)
{
    const QString nativeDirectory = QDir::toNativeSeparators(directory);
    if (!m_lineEdit->hasFocus()) {
        m_lineEdit->setText(nativeDirectory);
    }
    m_lineEdit->setToolTip(nativeDirectory);
    m_comboBox->setToolTip(nativeDirectory);
}
//=============================================================================
void
CurrentDirectoryWidget::setDirectories(const QStringList& directories)
{
    const QString currentText = m_lineEdit->text();
    const QSignalBlocker blocker(m_comboBox);
    m_comboBox->clear();
    for (const QString& directory : directories) {
        m_comboBox->addItem(QDir::toNativeSeparators(directory), directory);
    }
    m_comboBox->setEditText(currentText);
}
//=============================================================================
bool
CurrentDirectoryWidget::isValidDirectory(const QString& directory)
{
    return !directory.trimmed().isEmpty() && QDir(directory).exists();
}
//=============================================================================
