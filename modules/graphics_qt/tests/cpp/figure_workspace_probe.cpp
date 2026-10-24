//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QJsonDocument>
#include <QtCore/QFile>
#include <QtCore/QJsonObject>
#include <QtCore/QThread>
#include <QtCore/QTimer>
#include <QtCore/QDebug>
#include <QtGui/QAction>
#include <QtGui/QIcon>
#include <QtGui/QKeyEvent>
#include <QtGui/QWheelEvent>
#include <QtGui/QWindow>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QMenu>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QDialog>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QLayout>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QSlider>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QDoubleSpinBox>
#include <QtWidgets/QStyledItemDelegate>
#include <cmath>
//=============================================================================
int
armFigureFrames(const QString& title, const QJsonObject& request);
//=============================================================================
namespace {
int
probeWidget(QMainWindow* window, const QJsonObject& request)
{
    if (request["operation"] == "propertyRetained") {
        auto* tree = window->findChild<QTreeWidget*>("figureProperties");
        if (!tree) {
            return -2;
        }
        for (QTreeWidgetItemIterator iterator(tree); *iterator; ++iterator) {
            if ((*iterator)->text(0) == request["name"].toString()) {
                if (request["mark"].toBool()) {
                    (*iterator)->setData(0, Qt::UserRole + 1, true);
                }
                return (*iterator)->data(0, Qt::UserRole + 1).toBool() ? 1 : 0;
            }
        }
        return -3;
    }
    auto* widget = window->findChild<QWidget*>(request["name"].toString());
    if (!widget) {
        return -2;
    }
    if (request["operation"] == "textContains") {
        auto* label = qobject_cast<QLabel*>(widget);
        if (!label) {
            return -2;
        }
        const bool matches = label->text().contains(request["value"].toString());
        if (!matches) {
            qWarning().noquote() << "Qt label:" << label->text();
        }
        return matches ? 1 : 0;
    }
    return widget->isVisible() ? 1 : 0;
}
//=============================================================================
int
probeStatus(QMainWindow* window, const QJsonObject& request)
{
    auto* status = window->findChild<QLabel*>("figureActionStatus");
    if (!status) {
        return -2;
    }
    if (request["operation"] == "statusEmpty") {
        return status->text().isEmpty() ? 1 : 0;
    }
    auto* timer = window->findChild<QTimer*>("figureRefreshTimer");
    if (!timer) {
        return -2;
    }
    const auto previous = status->text();
    timer->setProperty("userRequested", request["operation"] != "backgroundRefresh");
    QMetaObject::invokeMethod(timer, "timeout", Qt::DirectConnection);
    timer->setProperty("userRequested", false);
    if (!status->property("pending").toBool()) {
        return request["operation"] == "backgroundRefresh" ? 0 : -3;
    }
    return status->text() == previous ? 1 : 0;
}
//=============================================================================
QMainWindow*
findFigureWindow(const QString& title)
{
    for (auto* widget : QApplication::allWidgets()) {
        if (widget->windowTitle() == title && qobject_cast<QMainWindow*>(widget)) {
            return qobject_cast<QMainWindow*>(widget);
        }
    }
    return nullptr;
}
//=============================================================================
int
probeLayout(QMainWindow* window, const QJsonObject& request)
{
    auto* layout = window->layout();
    auto* canvas = window->centralWidget();
    if (!layout || !canvas) {
        return -2;
    }
    if (request["operation"] == "presented") {
        return window->isVisible() && canvas->isVisible() && canvas->width() > 0
                && canvas->height() > 0
            ? 1
            : 0;
    }
    qWarning() << "Qt figure layout:" << window->size() << layout->geometry()
               << "canvas:" << canvas->geometry() << "layout enabled:" << layout->isEnabled();
    const QJsonObject report { { "windowWidth", window->width() },
        { "windowHeight", window->height() }, { "layoutX", layout->geometry().x() },
        { "layoutY", layout->geometry().y() }, { "layoutWidth", layout->geometry().width() },
        { "layoutHeight", layout->geometry().height() }, { "canvasWidth", canvas->width() },
        { "canvasHeight", canvas->height() }, { "enabled", layout->isEnabled() } };
    QJsonObject geometry = report;
    if (auto* native = window->windowHandle()) {
        geometry["nativeWidth"] = native->width();
        geometry["nativeHeight"] = native->height();
    }
    QFile output(request["path"].toString());
    if (!output.fileName().isEmpty() && output.open(QIODevice::WriteOnly)) {
        output.write(QJsonDocument(geometry).toJson());
    }
    return layout->geometry().width() == window->contentsRect().width()
            && layout->geometry().bottom() == window->contentsRect().bottom()
        ? 1
        : 0;
}
//=============================================================================
int
probeUndockMenu(QMainWindow* window)
{
    auto* dock = qobject_cast<QDockWidget*>(window->parentWidget());
    if (!dock || !dock->titleBarWidget()) {
        return -2;
    }
    auto* menu = dock->titleBarWidget()->findChild<QMenu*>();
    if (!menu) {
        return -2;
    }
    QMetaObject::invokeMethod(menu, "aboutToShow", Qt::DirectConnection);
    for (auto* action : menu->actions()) {
        if (action->objectName() == "nelsonUndockAction") {
            action->trigger();
            return 1;
        }
    }
    return -2;
}
//=============================================================================
int
probeAction(QMainWindow* window, const QJsonObject& request)
{
    const auto operation = request["operation"].toString();
    auto* action = window->findChild<QAction*>(request["name"].toString());
    if (operation == "actionExists") {
        return action ? 1 : 0;
    }
    if (!action) {
        return -2;
    }
    if (operation == "icon") {
        return !action->icon().isNull() && !action->icon().pixmap(18, 18).isNull() ? 1 : 0;
    }
    if (operation == "checked") {
        return action->isChecked() ? 1 : 0;
    }
    if (operation == "actionText") {
        return action->text() == request["value"].toString() ? 1 : 0;
    }
    action->trigger();
    return 1;
}
//=============================================================================
int
probeControls(QMainWindow* window, const QJsonObject& request)
{
    const auto operation = request["operation"].toString();
    if (operation.startsWith("slider")) {
        auto* slider = window->findChild<QSlider*>(request["name"].toString());
        if (!slider) {
            return -2;
        }
        if (operation == "sliderValue") {
            if (request.contains("expected")) {
                return slider->value() == request["expected"].toInt() ? 1 : 0;
            }
            return slider->value();
        }
        if (operation == "sliderKey") {
            slider->setFocus();
            QKeyEvent press(QEvent::KeyPress, Qt::Key_Right, Qt::NoModifier);
            QKeyEvent release(QEvent::KeyRelease, Qt::Key_Right, Qt::NoModifier);
            QApplication::sendEvent(slider, &press);
            QApplication::sendEvent(slider, &release);
            return 1;
        }
        if (request["down"].toBool()) {
            slider->setSliderDown(true);
        }
        if (request.contains("value")) {
            const int value = qRound(request["value"].toDouble() * 2);
            if (slider->isSliderDown()) {
                slider->setSliderPosition(value);
            } else {
                slider->setValue(value);
            }
        }
        if (request.contains("down") && !request["down"].toBool()) {
            slider->setSliderDown(false);
        }
        return 1;
    }
    if (operation == "selectedObject") {
        auto* tree = window->findChild<QTreeWidget*>("figureObjectBrowser");
        auto* item = tree ? tree->currentItem() : nullptr;
        return item
                && static_cast<qlonglong>(item->data(0, Qt::UserRole).toDouble())
                    == static_cast<qlonglong>(request["handle"].toDouble())
            ? 1
            : 0;
    }
    auto* marker = window->findChild<QWidget*>("figurePointMarker");
    auto* canvas = window->centralWidget();
    if (!marker || !canvas) {
        return -2;
    }
    const QPointF center = marker->geometry().center() - canvas->mapTo(window, QPoint());
    const QPointF expected(
        canvas->width() * request["x"].toDouble(), canvas->height() * request["y"].toDouble());
    return marker->parentWidget() == window && marker->isVisible()
            && marker->property("objectHandle").toDouble() == request["handle"].toDouble()
            && std::hypot(center.x() - expected.x(), center.y() - expected.y()) < 4
        ? 1
        : 0;
}
//=============================================================================
int
probeEnhancements(QMainWindow* window, const QJsonObject& request)
{
    const auto operation = request["operation"].toString();
    if (operation == "legendDoubleClick") {
        auto* action = window->findChild<QAction*>(request["name"].toString());
        auto* toolbar = window->findChild<QToolBar*>("figureLegendToolbar");
        auto* button = action && toolbar ? toolbar->widgetForAction(action) : nullptr;
        if (!button) {
            return -2;
        }
        const QPointF point = button->rect().center();
        for (const auto type : { QEvent::MouseButtonPress, QEvent::MouseButtonRelease,
                 QEvent::MouseButtonDblClick, QEvent::MouseButtonRelease }) {
            QMouseEvent event(type, point, button->mapToGlobal(point.toPoint()), Qt::LeftButton,
                Qt::LeftButton, Qt::NoModifier);
            QApplication::sendEvent(button, &event);
        }
        return 1;
    }
    if (operation == "controlKey") {
        auto* canvas = window->centralWidget();
        canvas->setFocus();
        const auto name = request["key"].toString();
        const int key = name == "Left" ? Qt::Key_Left
            : name == "Right"          ? Qt::Key_Right
            : name == "Up"             ? Qt::Key_Up
            : name == "Down"           ? Qt::Key_Down
                                       : Qt::Key_Z;
        const auto modifiers = request["undo"].toBool() ? Qt::ControlModifier
            : request["redo"].toBool()                  ? Qt::ControlModifier | Qt::ShiftModifier
                                                        : Qt::NoModifier;
        QKeyEvent press(QEvent::KeyPress, key, modifiers);
        QKeyEvent release(QEvent::KeyRelease, key, modifiers);
        QApplication::sendEvent(canvas, &press);
        QApplication::sendEvent(canvas, &release);
        return 1;
    }
    if (operation == "dockWidth") {
        auto* dock = window->findChild<QDockWidget*>("figurePropertiesDock");
        if (!dock) {
            return -2;
        }
        if (request.contains("value")) {
            window->resizeDocks({ dock }, { request["value"].toInt() }, Qt::Horizontal);
        }
        if (request.contains("expected")) {
            qWarning() << "Properties width:" << dock->width();
        }
        return request.contains("expected")
            ? (std::abs(dock->width() - request["expected"].toInt()) < 12 ? 1 : 0)
            : dock->width();
    }
    auto* tree = window->findChild<QTreeWidget*>("figureProperties");
    if (!tree) {
        return -2;
    }
    for (QTreeWidgetItemIterator iterator(tree); *iterator; ++iterator) {
        auto* item = *iterator;
        if (item->text(0) != request["name"].toString()) {
            continue;
        }
        const auto index = tree->indexFromItem(item, 1);
        auto* delegate = tree->itemDelegate();
        auto* editor = delegate->createEditor(tree, QStyleOptionViewItem(), index);
        if (!editor) {
            return -3;
        }
        delegate->setEditorData(editor, index);
        if (auto* combo = qobject_cast<QComboBox*>(editor)) {
            combo->setCurrentText(request["value"].toString());
        } else if (auto* spin = qobject_cast<QDoubleSpinBox*>(editor)) {
            spin->setValue(request["value"].toDouble());
        } else if (auto* color = editor->findChild<QComboBox*>("figureColorValue")) {
            color->setEditText(request["value"].toString());
        } else {
            delete editor;
            return -3;
        }
        delegate->setModelData(editor, tree->model(), index);
        delete editor;
        return 1;
    }
    return -3;
}
//=============================================================================
int
probe(const QString& title, const QJsonObject& request)
{
    auto* window = findFigureWindow(title);
    if (!window) {
        return -1;
    }
    const auto operation = request["operation"].toString();
    if (operation == "legendDoubleClick" || operation == "controlKey" || operation == "editor"
        || operation == "dockWidth") {
        return probeEnhancements(window, request);
    }
    if (operation == "refresh" || operation == "backgroundRefresh" || operation == "statusEmpty") {
        return probeStatus(window, request);
    }
    if (operation == "layout" || operation == "presented") {
        return probeLayout(window, request);
    }
    if (operation == "undockMenu") {
        return probeUndockMenu(window);
    }
    if (operation == "action" || operation == "checked" || operation == "icon"
        || operation == "actionExists" || operation == "actionText") {
        return probeAction(window, request);
    }
    if (operation.startsWith("slider") || operation == "selectedObject" || operation == "marker") {
        return probeControls(window, request);
    }
    if (operation == "status") {
        auto* status = window->findChild<QLabel*>("figureActionStatus");
        qWarning().noquote() << (status ? status->text() : "No status label");
    } else if (operation == "idle") {
        auto* status = window->findChild<QLabel*>("figureActionStatus");
        auto* motion = window->findChild<QTimer*>("figureMotionTimer");
        return status && !status->property("pending").toBool() && (!motion || !motion->isActive())
            ? 1
            : 0;
    } else if (operation == "visible" || operation == "textContains"
        || operation == "propertyRetained") {
        return probeWidget(window, request);
    } else if (operation == "indicator") {
        auto* widget = window->findChild<QWidget*>("figureAxesIndicator");
        return widget && widget->isVisible()
                && widget->property("axesHandle").toLongLong()
                    == static_cast<qlonglong>(request["handle"].toDouble())
            ? 1
            : 0;
    } else if (operation == "axes") {
        auto* selector = window->findChild<QComboBox*>("figureAxesSelector");
        if (!selector) {
            return -2;
        }
        const int index = request.contains("handle")
            ? selector->findData(QVariant::fromValue<qlonglong>(
                  static_cast<qlonglong>(request["handle"].toDouble())))
            : request["index"].toInt();
        if (index < 0 || index >= selector->count()) {
            return -2;
        }
        selector->setCurrentIndex(index);
        QMetaObject::invokeMethod(selector, "activated", Q_ARG(int, index));
    } else if (operation == "select") {
        auto* tree = window->findChild<QTreeWidget*>("figureObjectBrowser");
        if (!tree) {
            return -2;
        }
        for (QTreeWidgetItemIterator iterator(tree); *iterator; ++iterator) {
            if (static_cast<qlonglong>((*iterator)->data(0, Qt::UserRole).toDouble())
                == static_cast<qlonglong>(request["handle"].toDouble())) {
                tree->setCurrentItem(*iterator);
                return 1;
            }
        }
        return -3;
    } else if (operation == "property") {
        auto* tree = window->findChild<QTreeWidget*>("figureProperties");
        if (!tree) {
            return -2;
        }
        for (QTreeWidgetItemIterator iterator(tree); *iterator; ++iterator) {
            if ((*iterator)->text(0) == request["name"].toString()) {
                (*iterator)->setText(1, request["value"].toString());
                return 1;
            }
        }
        return -3;
    } else if (operation == "key") {
        const QString text = request["key"].toString();
        if (text.isEmpty()) {
            return -2;
        }
        auto* canvas = window->centralWidget();
        canvas->setFocus();
        const int key = text.at(0).toUpper().unicode();
        QKeyEvent press(QEvent::KeyPress, key, Qt::NoModifier, text);
        QKeyEvent release(QEvent::KeyRelease, key, Qt::NoModifier, text);
        QApplication::sendEvent(canvas, &press);
        QApplication::sendEvent(canvas, &release);
    } else if (operation == "wheel") {
        auto* canvas = window->centralWidget();
        const QPointF local = QPointF(canvas->mapTo(window, QPoint()))
            + QPointF(canvas->width() * request["x"].toDouble(),
                canvas->height() * request["y"].toDouble());
        const QPointF global = window->mapToGlobal(local);
        QWheelEvent wheel(local, global, QPoint(), QPoint(0, request["delta"].toInt()),
            Qt::NoButton, Qt::NoModifier, Qt::NoScrollPhase, false);
        QApplication::sendEvent(window, &wheel);
    } else if (operation == "snapshot") {
        return window->grab().save(request["path"].toString()) ? 1 : -2;
    } else if (operation == "save" || operation == "export") {
        const bool previous = QApplication::testAttribute(Qt::AA_DontUseNativeDialogs);
        QApplication::setAttribute(Qt::AA_DontUseNativeDialogs, true);
        const QString path = request["path"].toString();
        auto* timer = new QTimer(window);
        timer->setInterval(200);
        QObject::connect(timer, &QTimer::timeout, window, [path, timer]() {
            for (auto* widget : QApplication::topLevelWidgets()) {
                if (auto* dialog = qobject_cast<QFileDialog*>(widget)) {
                    if (!dialog->isVisible()) {
                        continue;
                    }
                    dialog->setOption(QFileDialog::DontConfirmOverwrite);
                    auto* edit = dialog->findChild<QLineEdit*>("fileNameEdit");
                    if (edit) {
                        edit->setText(path);
                    } else {
                        dialog->selectFile(path);
                    }
                    qWarning().noquote() << "Qt save dialog:" << dialog->selectedFiles();
                    QMetaObject::invokeMethod(dialog, "accept");
                    if (!dialog->isVisible()) {
                        timer->stop();
                    }
                }
            }
        });
        timer->start();
        // "save" drives the More-menu Save as; "export" drives the main toolbar Export
        // button, a separate code path (GOWindow::onExportAction, then callExportGraphicsGui)
        // that must also reach a save dialog.
        const QString actionName
            = operation == "export" ? "figureExportAction" : "figureSaveAsAction";
        auto* action = window->findChild<QAction*>(actionName);
        if (!action) {
            return -2;
        }
        action->trigger();
        timer->stop();
        timer->deleteLater();
        QApplication::setAttribute(Qt::AA_DontUseNativeDialogs, previous);
    } else if (operation == "print") {
        QTimer::singleShot(800, window, []() {
            for (auto* widget : QApplication::topLevelWidgets()) {
                if (widget->inherits("QPrintPreviewDialog")) {
                    QMetaObject::invokeMethod(widget, "reject");
                }
            }
        });
        auto* action = window->findChild<QAction*>("figurePrintAction");
        if (!action) {
            return -2;
        }
        action->trigger();
    } else {
        return -4;
    }
    return 1;
}
//=============================================================================
} // namespace
//=============================================================================
extern "C" Q_DECL_EXPORT int
figureWorkspaceProbe(const char* title, const char* encoded)
{
    const QString name = QString::fromUtf8(title);
    const auto request = QJsonDocument::fromJson(encoded).object();
    int result = -5;
    auto run = [&]() {
        result = request["operation"] == "armFrames" ? armFigureFrames(name, request)
                                                     : probe(name, request);
    };
    if (QThread::currentThread() == QApplication::instance()->thread()) {
        run();
    } else {
        QMetaObject::invokeMethod(QApplication::instance(), run, Qt::BlockingQueuedConnection);
    }
    return result;
}
//=============================================================================
