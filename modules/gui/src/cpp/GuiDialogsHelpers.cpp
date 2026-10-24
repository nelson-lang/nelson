//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QStringList>
#include <QtWidgets/QApplication>
#include <QtWidgets/QWidget>
#include "GuiDialogsHelpers.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "QObjectHandleObject.hpp"
#include "QStringConverter.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GuiDialogHelpers {
//=============================================================================
QWidget*
mainWindowParent()
{
    // The main window is looked up by its object name so that this helper does
    // not depend on the main window implementation.
    const QWidgetList topLevelWidgets = QApplication::topLevelWidgets();
    for (QWidget* widget : topLevelWidgets) {
        if (widget != nullptr && widget->objectName() == QLatin1String("NelsonMainWindow")) {
            return widget;
        }
    }
    return QApplication::activeWindow();
}
//=============================================================================
wstringVector
asTextList(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray()) {
        return { value.asWideString() };
    }
    if (value.isStringArray()) {
        return value.asWideStringVector(false);
    }
    if (value.isCellArrayOfCharacterVectors()) {
        return value.asWideStringVector();
    }
    Error(L"Nelson:gui:invalidArgument", L"'%s' expected.", L"Text");
    return {};
}
//=============================================================================
std::wstring
asText(const ArrayOf& value)
{
    wstringVector lines = asTextList(value);
    std::wstring text;
    for (size_t k = 0; k < lines.size(); ++k) {
        if (k != 0) {
            text += L"\n";
        }
        text += lines[k];
    }
    return text;
}
//=============================================================================
std::wstring
argText(const ArrayOfVector& args, size_t index, const std::wstring& defaultValue)
{
    return args.size() > index ? args[index].asWideString() : defaultValue;
}
//=============================================================================
bool
isName(const ArrayOf& value, const std::wstring& expected)
{
    return StringHelpers::iequals(value.asWideString(), expected);
}
//=============================================================================
QString
nelsonIconPath()
{
    QString root(wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()));
    return root + "/resources/fibonacci.ico";
}
//=============================================================================
ArrayOf
qObjectHandle(QObject* object, const QString& className)
{
    object->setProperty("Classname", className);
    return ArrayOf::handle(new QObjectHandleObject(object));
}
//=============================================================================
double
numericElement(const ArrayOf& value, indexType index, double defaultValue)
{
    if (value.elementCount() <= index) {
        return defaultValue;
    }
    std::vector<double> values = value.asDoubleVector();
    return values[index];
}
//=============================================================================
QString
fileFilter(const ArrayOfVector& argIn)
{
    if (argIn.empty()) {
        return QString("All Files (*)");
    }
    ArrayOf filter = argIn[0];
    if (filter.isCellArrayOfCharacterVectors()) {
        wstringVector parts = filter.asWideStringVector();
        if (parts.empty()) {
            return QString("All Files (*)");
        }
        Dimensions dims = filter.dimensions();
        const size_t rows = (size_t)dims.rows();
        const size_t columns = (size_t)dims.columns();
        QStringList filters;
        if (parts.size() == rows * columns && columns == 2 && rows >= 1) {
            // Nx2 cell: { pattern, description ; ... } (column major storage)
            for (size_t row = 0; row < rows; ++row) {
                const std::wstring pattern = parts[row];
                const std::wstring description = parts[rows + row];
                if (description.empty()) {
                    filters << wstringToQString(pattern);
                } else {
                    filters << wstringToQString(description) + " (" + wstringToQString(pattern)
                            + ")";
                }
            }
        } else {
            // Vector of filters: each entry is used as is
            for (const std::wstring& part : parts) {
                filters << wstringToQString(part);
            }
        }
        filters.removeAll(QString());
        if (filters.isEmpty()) {
            return QString("All Files (*)");
        }
        return filters.join(";;");
    }
    if (filter.isRowVectorCharacterArray() || filter.isStringArray()) {
        return wstringToQString(filter.asWideString());
    }
    return QString("All Files (*)");
}
//=============================================================================
QMessageBox::Icon
iconFromText(const std::wstring& icon)
{
    if (StringHelpers::iequals(icon, L"warning")) {
        return QMessageBox::Warning;
    }
    if (StringHelpers::iequals(icon, L"error")) {
        return QMessageBox::Critical;
    }
    if (StringHelpers::iequals(icon, L"success") || StringHelpers::iequals(icon, L"info")) {
        return QMessageBox::Information;
    }
    return QMessageBox::NoIcon;
}
//=============================================================================
FunctionDef*
findFunction(const std::string& name)
{
    FunctionDef* funcDef = nullptr;
    if (!PathFunctionIndexerManager::getInstance()->find(name, funcDef)) {
        BuiltInFunctionDefManager::getInstance()->find(name, funcDef);
    }
    if (funcDef == nullptr) {
        Error(L"Nelson:UI:functionNotFound", L"Required function not found.");
    }
    return funcDef;
}
//=============================================================================
} // namespace Nelson::GuiDialogHelpers
//=============================================================================
