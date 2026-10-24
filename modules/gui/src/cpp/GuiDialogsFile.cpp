//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtWidgets/QColorDialog>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QFontDialog>
#ifdef _MSC_VER
#include "ForceWindowsTitleBarToDark.hpp"
#endif
#include "GuiDialogs.hpp"
#include "GuiDialogProvider.hpp"
#include "i18n.hpp"
#include "GuiDialogsHelpers.hpp"
#include "QStringConverter.hpp"
#include "DialogDirectories.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static QStringList
runFileDialog(bool saveMode, const QString& title, const QString& path, const QString& filter,
    bool multiSelect, QString& selectedFilter);
//=============================================================================
static QColor
runColorDialog(const QColor& initial, const QString& title);
//=============================================================================
static QFont
runFontDialog(bool& ok, const QFont& initial, const QString& title);
//=============================================================================
ArrayOfVector
UiGetFile(const ArrayOfVector& argIn, bool saveMode)
{
    QString title = saveMode ? QString("Save File") : QString("Open File");
    if (argIn.size() > 1) {
        title = wstringToQString(argIn[1].asWideString());
    }
    QString selectedFilter;
    QString explicitPath;
    if (argIn.size() > 2) {
        explicitPath = wstringToQString(argIn[2].asWideString());
    }
    const std::wstring historyKey = saveMode ? L"desktop.save" : L"desktop.open";
    QString explicitDirectory = explicitPath;
    QString suggestedName;
    if (saveMode && !explicitPath.isEmpty() && explicitPath != QStringLiteral(".")) {
        QFileInfo info(explicitPath);
        if (!info.isDir()) {
            suggestedName = info.fileName();
            explicitDirectory
                = info.path() == QStringLiteral(".") ? QString() : info.absolutePath();
        }
    }
    const QString path = wstringToQString(resolveDialogDirectory(
        historyKey, QStringTowstring(explicitDirectory), QStringTowstring(QDir::currentPath())));
    const QString startPath = suggestedName.isEmpty() ? path : QDir(path).filePath(suggestedName);
    // Optional 'MultiSelect', 'on' | 'off' pairs (open mode only).
    bool multiSelect = false;
    if (!saveMode) {
        for (size_t k = 3; k + 1 < argIn.size(); k += 2) {
            if (!argIn[k].isRowVectorCharacterArray() && !argIn[k].isStringArray()) {
                continue;
            }
            if (GuiDialogHelpers::isName(argIn[k], L"MultiSelect")) {
                multiSelect = GuiDialogHelpers::isName(argIn[k + 1], L"on");
            }
        }
    }
    // Non-Qt front end (web): serve the file picker through the provider.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"file";
        request.title = QStringTowstring(title);
        request.fileMode = saveMode ? L"save" : L"open";
        request.defaultPath = QStringTowstring(path);
        request.suggestedName = QStringTowstring(suggestedName);
        request.historyKey = historyKey;
        request.fileFilters.push_back(QStringTowstring(GuiDialogHelpers::fileFilter(argIn)));
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            ArrayOfVector out;
            if (!response.ok || response.path.empty()) {
                out << ArrayOf::doubleScalar(0);
                out << ArrayOf::doubleScalar(0);
                out << ArrayOf::doubleScalar(0);
                return out;
            }
            QFileInfo picked(wstringToQString(response.path));
            setDialogDirectory(historyKey, QStringTowstring(picked.absolutePath()));
            out << ArrayOf::characterArray(QStringTowstring(picked.fileName()));
            out << ArrayOf::characterArray(
                QStringTowstring(QDir::toNativeSeparators(picked.path() + "/")));
            out << ArrayOf::doubleScalar(1);
            return out;
        }
    }
    QStringList files = runFileDialog(saveMode, title, startPath,
        GuiDialogHelpers::fileFilter(argIn), multiSelect, selectedFilter);
    ArrayOfVector retval;
    if (files.isEmpty()) {
        retval << ArrayOf::doubleScalar(0);
        retval << ArrayOf::doubleScalar(0);
        retval << ArrayOf::doubleScalar(0);
        return retval;
    }
    QFileInfo info(files.first());
    setDialogDirectory(historyKey, QStringTowstring(info.absolutePath()));
    if (multiSelect && files.size() > 1) {
        // MultiSelect only changes the first output when several files are
        // selected: a cell array of file names, as expected by callers.
        wstringVector names;
        for (const QString& file : files) {
            names.push_back(QStringTowstring(QFileInfo(file).fileName()));
        }
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(names);
    } else {
        retval << ArrayOf::characterArray(QStringTowstring(info.fileName()));
    }
    retval << ArrayOf::characterArray(
        QStringTowstring(QDir::toNativeSeparators(info.path() + "/")));
    retval << ArrayOf::doubleScalar(1);
    return retval;
}
//=============================================================================
ArrayOf
UiSetColor(const ArrayOfVector& argIn)
{
    QColor initial = Qt::white;
    if (!argIn.empty() && argIn[0].isNumeric() && argIn[0].elementCount() >= 3) {
        initial = QColor::fromRgbF(GuiDialogHelpers::numericElement(argIn[0], 0, 1),
            GuiDialogHelpers::numericElement(argIn[0], 1, 1),
            GuiDialogHelpers::numericElement(argIn[0], 2, 1));
    }
    QString title;
    if (argIn.size() > 1) {
        title = wstringToQString(argIn[1].asWideString());
    }
    // Non-Qt front end (web): serve the color picker through the provider.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"color";
        request.title = title.isEmpty() ? _W("Select a color") : QStringTowstring(title);
        request.defaultPath = QStringTowstring(initial.name());
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            if (!response.ok || response.path.empty()) {
                return ArrayOf::doubleScalar(0);
            }
            QColor picked(wstringToQString(response.path));
            if (!picked.isValid()) {
                return ArrayOf::doubleScalar(0);
            }
            return ArrayOf::doubleRowVector({ picked.redF(), picked.greenF(), picked.blueF() });
        }
    }
    QColor color = runColorDialog(initial, title);
    if (!color.isValid()) {
        return ArrayOf::doubleScalar(0);
    }
    return ArrayOf::doubleRowVector({ color.redF(), color.greenF(), color.blueF() });
}
//=============================================================================
ArrayOf
UiSetFont(const ArrayOfVector& argIn)
{
    bool ok = false;
    QFont initial;
    if (!argIn.empty() && argIn[0].isStruct() && argIn[0].isScalar()) {
        if (argIn[0].isField("FontName")) {
            initial.setFamily(wstringToQString(argIn[0].field("FontName").asWideString()));
        }
        if (argIn[0].isField("FontSize")) {
            initial.setPointSizeF(argIn[0].field("FontSize").asDoubleScalar());
        }
    }
    QString title;
    if (argIn.size() > 1) {
        title = wstringToQString(argIn[1].asWideString());
    }
    // Non-Qt front end (web): serve the font picker through the provider.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"font";
        request.title = title.isEmpty() ? _W("Select a font") : QStringTowstring(title);
        request.fontFamily = QStringTowstring(initial.family());
        request.fontSize = initial.pointSizeF() > 0 ? initial.pointSizeF() : 10.0;
        request.fontBold = initial.bold();
        request.fontItalic = initial.italic();
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            if (!response.ok || response.values.size() < 2) {
                return ArrayOf::doubleScalar(0);
            }
            double sz = 10.0;
            try {
                sz = std::stod(response.values[1]);
            } catch (...) {
            }
            ArrayOfVector fields;
            fields << ArrayOf::characterArray(response.values[0]);
            fields << ArrayOf::doubleScalar(sz);
            fields << ArrayOf::characterArray(
                response.values.size() > 2 ? response.values[2] : L"normal");
            fields << ArrayOf::characterArray(
                response.values.size() > 3 ? response.values[3] : L"normal");
            fields << ArrayOf::characterArray(L"points");
            return ArrayOf::structScalar(
                { "FontName", "FontSize", "FontWeight", "FontAngle", "FontUnits" }, fields);
        }
    }
    QFont font = runFontDialog(ok, initial, title);
    if (!ok) {
        return ArrayOf::doubleScalar(0);
    }
    ArrayOfVector values;
    values << ArrayOf::characterArray(QStringTowstring(font.family()));
    values << ArrayOf::doubleScalar(font.pointSizeF());
    values << ArrayOf::characterArray(font.bold() ? L"bold" : L"normal");
    values << ArrayOf::characterArray(font.italic() ? L"italic" : L"normal");
    values << ArrayOf::characterArray(L"points");
    return ArrayOf::structScalar(
        { "FontName", "FontSize", "FontWeight", "FontAngle", "FontUnits" }, values);
}
//=============================================================================
static QStringList
runFileDialog(bool saveMode, const QString& title, const QString& path, const QString& filter,
    bool multiSelect, QString& selectedFilter)
{
    QFileDialog dialog(GuiDialogHelpers::mainWindowParent(), title, path, filter);
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(dialog.winId());
#endif
    dialog.setAcceptMode(saveMode ? QFileDialog::AcceptSave : QFileDialog::AcceptOpen);
    if (saveMode) {
        dialog.setFileMode(QFileDialog::AnyFile);
    } else {
        dialog.setFileMode(multiSelect ? QFileDialog::ExistingFiles : QFileDialog::ExistingFile);
    }
    if (dialog.exec() != QDialog::Accepted || dialog.selectedFiles().isEmpty()) {
        return QStringList();
    }
    selectedFilter = dialog.selectedNameFilter();
    return dialog.selectedFiles();
}
//=============================================================================
static QColor
runColorDialog(const QColor& initial, const QString& title)
{
    QColorDialog dialog(initial, GuiDialogHelpers::mainWindowParent());
    if (!title.isEmpty()) {
        dialog.setWindowTitle(title);
    }
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(dialog.winId());
#endif
    if (dialog.exec() != QDialog::Accepted) {
        return QColor();
    }
    return dialog.selectedColor();
}
//=============================================================================
static QFont
runFontDialog(bool& ok, const QFont& initial, const QString& title)
{
    QFontDialog dialog(initial, GuiDialogHelpers::mainWindowParent());
    if (!title.isEmpty()) {
        dialog.setWindowTitle(title);
    }
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(dialog.winId());
#endif
    ok = dialog.exec() == QDialog::Accepted;
    return ok ? dialog.selectedFont() : QFont();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
