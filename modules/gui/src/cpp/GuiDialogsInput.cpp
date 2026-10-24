//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QFontMetrics>
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QVBoxLayout>
#include <vector>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GuiDialogs.hpp"
#include "GuiDialogProvider.hpp"
#include "GuiDialogsHelpers.hpp"
#include "QStringConverter.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
#ifdef _MSC_VER
#include "ForceWindowsTitleBarToDark.hpp"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
InputDialog(const ArrayOfVector& argIn)
{
    wstringVector prompts = GuiDialogHelpers::asTextList(argIn[0]);
    std::wstring title = GuiDialogHelpers::argText(argIn, 1, _W("Input Dialog"));
    // Third argument: number of lines of each edit field (scalar or per prompt).
    std::vector<double> numLines;
    if (argIn.size() > 2 && argIn[2].isNumeric() && !argIn[2].isEmpty()) {
        numLines = argIn[2].asDoubleVector();
    }
    wstringVector defaults;
    if (argIn.size() > 3) {
        defaults = GuiDialogHelpers::asTextList(argIn[3]);
    }

    // Non-Qt front end (web): serve the dialog through the registered provider.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"input";
        request.title = title;
        for (size_t k = 0; k < prompts.size(); ++k) {
            GuiDialogField field;
            field.prompt = prompts[k];
            field.value = defaults.size() > k ? defaults[k] : std::wstring();
            field.lines = 1;
            if (!numLines.empty()) {
                double requested = numLines.size() > k ? numLines[k] : numLines[0];
                if (requested >= 1.0 && requested < 100.0) {
                    field.lines = static_cast<int>(requested);
                }
            }
            request.fields.push_back(field);
        }
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            if (!response.ok) {
                return ArrayOf::emptyCell(Dimensions(0, 0));
            }
            ArrayOfMatrix matrix;
            for (size_t k = 0; k < prompts.size(); ++k) {
                ArrayOfVector row;
                row << ArrayOf::characterArray(
                    response.values.size() > k ? response.values[k] : std::wstring());
                matrix.push_back(row);
            }
            return ArrayOf::cellArray(matrix);
        }
    }

    QDialog dialog(GuiDialogHelpers::mainWindowParent());
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(dialog.winId());
#endif
    dialog.setWindowTitle(wstringToQString(title));

    QVBoxLayout* mainLayout = new QVBoxLayout(&dialog);
    QFormLayout* formLayout = new QFormLayout();
    mainLayout->addLayout(formLayout);

    std::vector<QLineEdit*> lineEdits(prompts.size(), nullptr);
    std::vector<QPlainTextEdit*> textEdits(prompts.size(), nullptr);

    for (size_t k = 0; k < prompts.size(); ++k) {
        QString defaultText = defaults.size() > k ? wstringToQString(defaults[k]) : QString();
        int lines = 1;
        if (!numLines.empty()) {
            double requested = numLines.size() > k ? numLines[k] : numLines[0];
            if (requested >= 1.0 && requested < 100.0) {
                lines = static_cast<int>(requested);
            }
        }
        if (lines > 1) {
            QPlainTextEdit* edit = new QPlainTextEdit(defaultText, &dialog);
            QFontMetrics metrics(edit->font());
            edit->setMinimumHeight(metrics.lineSpacing() * lines + metrics.height());
            textEdits[k] = edit;
            formLayout->addRow(wstringToQString(prompts[k]), edit);
        } else {
            QLineEdit* edit = new QLineEdit(defaultText, &dialog);
            lineEdits[k] = edit;
            formLayout->addRow(wstringToQString(prompts[k]), edit);
        }
    }

    QDialogButtonBox* buttons
        = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
    QObject::connect(buttons, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    QObject::connect(buttons, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
    mainLayout->addWidget(buttons);

    if (!lineEdits.empty() && lineEdits[0] != nullptr) {
        lineEdits[0]->setFocus();
    }

    if (dialog.exec() != QDialog::Accepted) {
        return ArrayOf::emptyCell(Dimensions(0, 0));
    }

    ArrayOfMatrix matrix;
    for (size_t k = 0; k < prompts.size(); ++k) {
        QString text;
        if (textEdits[k] != nullptr) {
            text = textEdits[k]->toPlainText();
        } else if (lineEdits[k] != nullptr) {
            text = lineEdits[k]->text();
        }
        ArrayOfVector row;
        row << ArrayOf::characterArray(QStringTowstring(text));
        matrix.push_back(row);
    }
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
ArrayOfVector
ListDialog(const ArrayOfVector& argIn)
{
    wstringVector items;
    std::wstring title = _W("List Dialog");
    std::wstring prompt;
    bool multiple = false;
    std::vector<double> initial;
    for (size_t k = 0; k + 1 < argIn.size(); k += 2) {
        if (GuiDialogHelpers::isName(argIn[k], L"ListString")) {
            items = GuiDialogHelpers::asTextList(argIn[k + 1]);
        } else if (GuiDialogHelpers::isName(argIn[k], L"Name")) {
            title = argIn[k + 1].asWideString();
        } else if (GuiDialogHelpers::isName(argIn[k], L"PromptString")) {
            prompt = GuiDialogHelpers::asText(argIn[k + 1]);
        } else if (GuiDialogHelpers::isName(argIn[k], L"SelectionMode")) {
            multiple = StringHelpers::iequals(argIn[k + 1].asWideString(), L"multiple");
        } else if (GuiDialogHelpers::isName(argIn[k], L"InitialValue")) {
            initial = argIn[k + 1].asDoubleVector();
        }
    }
    if (items.empty()) {
        Error(L"Nelson:listdlg:listStringExpected", L"'%s' expected.", L"ListString");
    }

    // Non-Qt front end (web): serve the list dialog through the provider.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"list";
        request.title = title;
        request.message = prompt;
        request.items = items;
        request.multiSelect = multiple;
        for (double value : initial) {
            request.initialSelection.push_back(static_cast<int>(value));
        }
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            ArrayOfVector retval;
            if (!response.ok) {
                retval << ArrayOf::empty(0, 0);
                retval << ArrayOf::logicalScalar(false);
                return retval;
            }
            std::vector<double> selected;
            for (int idx : response.selectedIndices) {
                selected.push_back((double)idx);
            }
            retval << ArrayOf::doubleRowVector(selected);
            retval << ArrayOf::logicalScalar(true);
            return retval;
        }
    }

    QDialog dialog(GuiDialogHelpers::mainWindowParent());
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(dialog.winId());
#endif
    dialog.setWindowTitle(wstringToQString(title));
    QVBoxLayout layout(&dialog);
    if (!prompt.empty()) {
        layout.addWidget(new QLabel(wstringToQString(prompt)));
    }
    QListWidget list;
    // ExtendedSelection keeps the usual Ctrl/Shift semantics instead of the
    // toggle-on-click behaviour of MultiSelection.
    list.setSelectionMode(
        multiple ? QAbstractItemView::ExtendedSelection : QAbstractItemView::SingleSelection);
    for (const std::wstring& item : items) {
        list.addItem(wstringToQString(item));
    }
    for (double value : initial) {
        int row = static_cast<int>(value) - 1;
        if (row >= 0 && row < list.count()) {
            list.item(row)->setSelected(true);
        }
    }
    layout.addWidget(&list);
    QObject::connect(&list, &QListWidget::itemDoubleClicked, &dialog,
        [&dialog](QListWidgetItem*) { dialog.accept(); });

    QHBoxLayout* selectionLayout = nullptr;
    if (multiple) {
        selectionLayout = new QHBoxLayout();
        QPushButton* selectAllButton = new QPushButton(wstringToQString(_W("Select all")), &dialog);
        QPushButton* clearButton = new QPushButton(wstringToQString(_W("Clear")), &dialog);
        QObject::connect(
            selectAllButton, &QPushButton::clicked, &list, [&list]() { list.selectAll(); });
        QObject::connect(
            clearButton, &QPushButton::clicked, &list, [&list]() { list.clearSelection(); });
        selectionLayout->addWidget(selectAllButton);
        selectionLayout->addWidget(clearButton);
        selectionLayout->addStretch();
        layout.addLayout(selectionLayout);
    }

    QDialogButtonBox buttons(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
    QObject::connect(&buttons, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    QObject::connect(&buttons, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
    layout.addWidget(&buttons);
    ArrayOfVector retval;
    if (dialog.exec() != QDialog::Accepted) {
        retval << ArrayOf::empty(0, 0);
        retval << ArrayOf::logicalScalar(false);
        return retval;
    }
    std::vector<double> selected;
    for (int k = 0; k < list.count(); ++k) {
        if (list.item(k)->isSelected()) {
            selected.push_back((double)k + 1);
        }
    }
    retval << ArrayOf::doubleRowVector(selected);
    retval << ArrayOf::logicalScalar(true);
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
