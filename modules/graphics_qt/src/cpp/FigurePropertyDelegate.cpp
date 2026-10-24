//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FigurePropertyDelegate.hpp"
#include "FigureWorkspaceHelpers.hpp"
#include <QtWidgets/QColorDialog>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDoubleSpinBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QToolButton>
//=============================================================================
namespace Nelson {
QWidget*
FigurePropertyDelegate::createEditor(
    QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
    if (index.column() != 1) {
        return nullptr;
    }
    const QString name = index.siblingAtColumn(0).data().toString();
    const QString kind = index.data(RolePropertyKind).toString();
    QStringList modelChoices = index.data(RolePropertyChoices).toStringList();
    if (kind == "color" || (kind.isEmpty() && name.endsWith("Color"))) {
        auto* editor = new QComboBox(parent);
        editor->setObjectName("figureColorEditor");
        auto* value = editor;
        value->setEditable(true);
        value->addItems({ "red", "green", "blue", "black", "white", "yellow", "cyan", "magenta" });
        for (const QString& choice : modelChoices) {
            if (value->findText(choice) < 0) {
                value->addItem(choice);
            }
        }
        if (modelChoices.isEmpty() && name != "Color") {
            value->addItems({ "none", "auto" });
            if (!name.startsWith("Marker")) {
                value->addItems({ "flat", "interp" });
            }
        }
        QPixmap swatch(16, 16);
        swatch.fill(QColor(25, 118, 210));
        auto* picker = editor->lineEdit()->addAction(QIcon(swatch), QLineEdit::LeadingPosition);
        picker->setObjectName("figureColorPicker");
        picker->setText(tr("Choose color"));
        picker->setToolTip(tr("Choose color"));
        auto* delegate = const_cast<FigurePropertyDelegate*>(this);
        connect(picker, &QAction::triggered, editor, [value, editor, delegate]() {
            bool valid = false;
            const auto rgb
                = parseFigureProperty(value->currentText(), QJsonArray(), valid, true).toArray();
            const auto initial = valid
                ? QColor::fromRgbF(rgb[0].toDouble(), rgb[1].toDouble(), rgb[2].toDouble())
                : QColor(value->currentText());
            const auto color = QColorDialog::getColor(initial, editor, tr("Choose color"));
            if (color.isValid()) {
                value->setEditText(
                    QString("[%1,%2,%3]").arg(color.redF()).arg(color.greenF()).arg(color.blueF()));
                emit delegate->commitData(editor);
                emit delegate->closeEditor(editor);
            }
        });
        return editor;
    }
    if (kind == "number" || (kind.isEmpty() && (name == "LineWidth" || name == "MarkerSize"))) {
        auto* editor = new QDoubleSpinBox(parent);
        editor->setRange(-1e9, 1e9);
        editor->setDecimals(4);
        editor->setSingleStep(name == "LineWidth" ? 0.5 : 1);
        return editor;
    }
    QStringList choices = modelChoices;
    if (kind == "bool" && choices.isEmpty()) {
        choices = { "true", "false" };
    }
    if (choices.isEmpty() && kind.isEmpty()) {
        if (name == "LineStyle") {
            choices = { "-", "--", ":", "-.", "none" };
        } else if (name == "Marker") {
            choices = { "none", "+", "o", "*", ".", "x", "square", "diamond", "^", "v", ">", "<",
                "pentagram", "hexagram" };
        } else if (name.endsWith("Mode")) {
            choices = { "auto", "manual" };
        } else if (name.endsWith("Scale")) {
            choices = { "linear", "log" };
        } else if (name.endsWith("Dir")) {
            choices = { "normal", "reverse" };
        } else if (name == "Projection") {
            choices = { "orthographic", "perspective" };
        } else if (name.endsWith("Grid") || name == "Visible" || name == "NumberTitle") {
            choices = { "on", "off" };
        }
    }
    if (choices.isEmpty()) {
        return QStyledItemDelegate::createEditor(parent, option, index);
    }
    auto* editor = new QComboBox(parent);
    editor->addItems(choices);
    return editor;
}
//=============================================================================
void
FigurePropertyDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const
{
    if (editor->objectName() == "figureColorEditor") {
        qobject_cast<QComboBox*>(editor)->setEditText(index.data().toString());
    } else if (auto* choices = qobject_cast<QComboBox*>(editor)) {
        choices->setCurrentText(index.data().toString());
    } else if (auto* number = qobject_cast<QDoubleSpinBox*>(editor)) {
        number->setValue(index.data().toDouble());
    } else {
        QStyledItemDelegate::setEditorData(editor, index);
    }
}
//=============================================================================
void
FigurePropertyDelegate::setModelData(
    QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const
{
    if (editor->objectName() == "figureColorEditor") {
        model->setData(index, qobject_cast<QComboBox*>(editor)->currentText(), Qt::EditRole);
    } else if (auto* choices = qobject_cast<QComboBox*>(editor)) {
        model->setData(index, choices->currentText(), Qt::EditRole);
    } else if (auto* number = qobject_cast<QDoubleSpinBox*>(editor)) {
        model->setData(index, QString::number(number->value()), Qt::EditRole);
    } else {
        QStyledItemDelegate::setModelData(editor, model, index);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
