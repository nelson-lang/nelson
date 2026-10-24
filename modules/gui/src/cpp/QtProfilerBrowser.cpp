//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <tuple>
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QTimer>
#include <QtCore/QUrl>
#include <QtGui/QDesktopServices>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QVBoxLayout>
#include "QtProfilerBrowser.h"
#include "QtDockTitleBar.h"
#include "QtTranslation.hpp"
#include "NelsonConfiguration.hpp"
#include "QtFileDialogHistory.hpp"
#include "PostCommand.hpp"
#include "Profiler.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static QString
resourcePath(const QString& name)
{
    return wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory())
        + QString("/resources/") + name;
}
//=============================================================================
static std::wstring
quoteNelsonString(const QString& text)
{
    QString quoted(text);
    quoted.replace("'", "''");
    return QStringTowstring(quoted);
}
//=============================================================================
class NumericTableWidgetItem : public QTableWidgetItem
{
public:
    explicit NumericTableWidgetItem(double value, const QString& text) : QTableWidgetItem(text)
    {
        setData(Qt::UserRole, value);
        setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
    }

    bool
    operator<(const QTableWidgetItem& other) const override
    {
        bool okLeft = false;
        bool okRight = false;
        double left = data(Qt::UserRole).toDouble(&okLeft);
        double right = other.data(Qt::UserRole).toDouble(&okRight);
        if (okLeft && okRight) {
            return left < right;
        }
        return QTableWidgetItem::operator<(other);
    }
};
//=============================================================================
static QTableWidgetItem*
textItem(const QString& text)
{
    QTableWidgetItem* item = new QTableWidgetItem(text);
    item->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    return item;
}
//=============================================================================
static QTableWidgetItem*
numericItem(double value, int precision)
{
    return new NumericTableWidgetItem(value, QString::number(value, 'f', precision));
}
//=============================================================================
QtProfilerBrowser::QtProfilerBrowser(QWidget* parent)
    : QDockWidget(TR("Profiler"), parent)
    , m_toolBar(nullptr)
    , m_runAction(nullptr)
    , m_clearAction(nullptr)
    , m_exportAction(nullptr)
    , m_refreshAction(nullptr)
    , m_stateLabel(nullptr)
    , m_table(nullptr)
    , m_stateTimer(nullptr)
    , m_lastProfilerState(false)
{
    setObjectName("profilerBrowser");
    setupUi();
    setTitleBarWidget(new QtDockTitleBar(this, "pb"));
    m_lastProfilerState = Profiler::getInstance()->isOn();
    m_stateTimer = new QTimer(this);
    m_stateTimer->setInterval(500);
    connect(m_stateTimer, &QTimer::timeout, this, &QtProfilerBrowser::syncProfilerState);
    m_stateTimer->start();
    refreshProfiler();
    hide();
}
//=============================================================================
void
QtProfilerBrowser::setupUi()
{
    QWidget* centralWidget = new QWidget(this);
    QVBoxLayout* layout = new QVBoxLayout(centralWidget);
    layout->setContentsMargins(4, 4, 4, 4);
    layout->setSpacing(4);

    m_toolBar = new QToolBar(TR("Profiler Toolbar"), centralWidget);
    m_toolBar->setObjectName("ProfilerToolBar");
    m_toolBar->setIconSize(QSize(18, 18));
    m_toolBar->setMovable(false);
    m_toolBar->setFloatable(false);
    m_toolBar->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

    m_runAction = new QAction(this);
    connect(m_runAction, &QAction::triggered, this, &QtProfilerBrowser::toggleProfiler);
    m_toolBar->addAction(m_runAction);

    m_clearAction = new QAction(QIcon(resourcePath("user-trash.svg")), TR("Clear"), this);
    m_clearAction->setToolTip(TR("Clear profiler data"));
    connect(m_clearAction, &QAction::triggered, this, &QtProfilerBrowser::clearProfiler);
    m_toolBar->addAction(m_clearAction);

    m_exportAction = new QAction(QIcon(resourcePath("document-save-as.svg")), TR("Export"), this);
    m_exportAction->setToolTip(TR("Export profiler report"));
    connect(m_exportAction, &QAction::triggered, this, &QtProfilerBrowser::exportProfiler);
    m_toolBar->addAction(m_exportAction);

    m_refreshAction = new QAction(QIcon(resourcePath("refresh.svg")), TR("Refresh"), this);
    m_refreshAction->setShortcut(QKeySequence::Refresh);
    m_refreshAction->setToolTip(TR("Refresh profiler data"));
    connect(m_refreshAction, &QAction::triggered, this, &QtProfilerBrowser::refreshProfiler);
    m_toolBar->addAction(m_refreshAction);

    m_stateLabel = new QLabel(m_toolBar);
    m_stateLabel->setObjectName("ProfilerStateLabel");
    m_toolBar->addWidget(m_stateLabel);

    layout->addWidget(m_toolBar);

    m_table = new QTableWidget(centralWidget);
    m_table->setColumnCount(5);
    m_table->setHorizontalHeaderLabels(
        { TR("Function"), TR("Line"), TR("Calls"), TR("Total (s)"), TR("Per call (s)") });
    m_table->setSelectionBehavior(QAbstractItemView::SelectRows);
    m_table->setSelectionMode(QAbstractItemView::SingleSelection);
    m_table->setEditTriggers(QAbstractItemView::NoEditTriggers);
    m_table->setAlternatingRowColors(true);
    m_table->setSortingEnabled(true);
    m_table->verticalHeader()->setVisible(false);
    m_table->horizontalHeader()->setStretchLastSection(false);
    m_table->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
    for (int k = 1; k < m_table->columnCount(); ++k) {
        m_table->horizontalHeader()->setSectionResizeMode(k, QHeaderView::ResizeToContents);
    }
    connect(m_table, &QTableWidget::cellDoubleClicked, this, &QtProfilerBrowser::openProfileRow);
    layout->addWidget(m_table, 1);

    setWidget(centralWidget);
}
//=============================================================================
void
QtProfilerBrowser::updateRunAction()
{
    if (!m_runAction || !m_stateLabel) {
        return;
    }
    bool running = Profiler::getInstance()->isOn();
    if (m_exportAction) {
        m_exportAction->setEnabled(!running);
    }
    if (running) {
        m_runAction->setIcon(QIcon(resourcePath("stop-interpreter.svg")));
        m_runAction->setText(TR("Stop"));
        m_runAction->setToolTip(TR("Stop profiling"));
        m_stateLabel->setText(QStringLiteral(" ") + TR("profiling..."));
    } else {
        m_runAction->setIcon(QIcon(resourcePath("file-run.svg")));
        m_runAction->setText(TR("Start"));
        m_runAction->setToolTip(TR("Start profiling"));
        m_stateLabel->setText(QString());
    }
}
//=============================================================================
void
QtProfilerBrowser::toggleProfiler()
{
    if (Profiler::getInstance()->isOn()) {
        Profiler::getInstance()->off();
        m_lastProfilerState = false;
        refreshProfiler();
    } else {
        Profiler::getInstance()->on();
        m_lastProfilerState = true;
        m_table->setRowCount(0);
        updateRunAction();
    }
}
//=============================================================================
void
QtProfilerBrowser::clearProfiler()
{
    Profiler::getInstance()->clear();
    m_table->setRowCount(0);
    m_lastProfilerState = Profiler::getInstance()->isOn();
    updateRunAction();
}
//=============================================================================
void
QtProfilerBrowser::exportProfiler()
{
    if (Profiler::getInstance()->isOn()) {
        return;
    }
    auto lines = Profiler::getInstance()->info(Profiler::Profile_Sort_Type::SORT_BY_NAMEFILELINE);
    if (lines.empty()) {
        QMessageBox::information(this, TR("Profiler"), TR("No profile data to export."));
        updateRunAction();
        return;
    }

    QFileDialog dialog(
        this, TR("Export Profiler Report"), qtDialogInitialDirectory(L"desktop.directory"));
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setOption(QFileDialog::ShowDirsOnly, true);
    dialog.selectFile("profile_result");
    if (dialog.exec() != QDialog::Accepted || dialog.selectedFiles().isEmpty()) {
        return;
    }

    QString destination = dialog.selectedFiles().first();
    qtRecordDialogSelection(L"desktop.directory", QStringLiteral("dir"), destination);
    std::wstring errorMessage;
    std::wstring moduleProfilerPath
        = NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/modules/profiler";
    Profiler::getInstance()->save(
        lines, QStringTowstring(destination), moduleProfilerPath, errorMessage);
    if (!errorMessage.empty()) {
        QMessageBox::warning(this, TR("Profiler"), wstringToQString(errorMessage));
        return;
    }

    QDesktopServices::openUrl(QUrl::fromLocalFile(QDir(destination).filePath("index.html")));
}
//=============================================================================
void
QtProfilerBrowser::refreshProfiler()
{
    updateRunAction();
    m_table->setSortingEnabled(false);
    m_table->clearContents();

    auto lines = Profiler::getInstance()->info(Profiler::Profile_Sort_Type::SORT_BY_TOTALTIME);
    std::reverse(lines.begin(), lines.end());
    m_table->setRowCount((int)lines.size());

    int row = 0;
    for (const auto& line : lines) {
        QString filename = wstringToQString(utf8_to_wstring(std::get<0>(line)));
        QString functionName = wstringToQString(utf8_to_wstring(std::get<2>(line)));
        double lineNumber = (double)std::get<1>(line);
        double calls = (double)std::get<3>(line);
        double total = (double)std::get<4>(line) * 1e-9;
        double perCall = (double)std::get<5>(line) * 1e-9;

        QTableWidgetItem* functionItem = textItem(functionName);
        functionItem->setData(Qt::UserRole, filename);
        functionItem->setData(Qt::UserRole + 1, (int)lineNumber);
        if (!filename.isEmpty()) {
            functionItem->setToolTip(
                QString("%1:%2").arg(QDir::toNativeSeparators(filename)).arg((int)lineNumber));
        }
        m_table->setItem(row, 0, functionItem);
        m_table->setItem(row, 1, numericItem(lineNumber, 0));
        m_table->setItem(row, 2, numericItem(calls, 0));
        m_table->setItem(row, 3, numericItem(total, total < 1. ? 6 : 3));
        m_table->setItem(row, 4, numericItem(perCall, perCall < 1. ? 6 : 3));
        ++row;
    }

    m_table->setSortingEnabled(true);
    m_table->sortItems(3, Qt::DescendingOrder);
    m_lastProfilerState = Profiler::getInstance()->isOn();
    updateRunAction();
}
//=============================================================================
void
QtProfilerBrowser::syncProfilerState()
{
    bool running = Profiler::getInstance()->isOn();
    if (m_lastProfilerState && !running) {
        m_lastProfilerState = false;
        refreshProfiler();
        return;
    }
    if (!m_lastProfilerState && running) {
        m_lastProfilerState = true;
        m_table->setRowCount(0);
    }
    updateRunAction();
}
//=============================================================================
void
QtProfilerBrowser::openProfileRow(int row, int column)
{
    (void)column;
    QTableWidgetItem* item = m_table->item(row, 0);
    if (!item) {
        return;
    }
    QString filename = item->data(Qt::UserRole).toString();
    int line = item->data(Qt::UserRole + 1).toInt();
    if (filename.isEmpty()) {
        return;
    }
    QFileInfo fileInfo(filename);
    if (!fileInfo.exists()) {
        QDesktopServices::openUrl(QUrl::fromLocalFile(filename));
        return;
    }
    postCommand(L"editor('" + quoteNelsonString(fileInfo.absoluteFilePath()) + L"', "
        + std::to_wstring(std::max(1, line)) + L")");
}
//=============================================================================
void
QtProfilerBrowser::closeEvent(QCloseEvent* event)
{
    emit closeProfilerBrowser();
    QDockWidget::closeEvent(event);
}
//=============================================================================
