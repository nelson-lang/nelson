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
#include <QtWidgets/QMessageBox>
#include <string>
#include "ArrayOf.hpp"
#include "FunctionDef.hpp"
//=============================================================================
class QObject;
class QWidget;
//=============================================================================
namespace Nelson::GuiDialogHelpers {
//=============================================================================
// Returns the Nelson main window (or the current active window as a fallback)
// so that dialogs are centered on it and never end up behind it.
// Returns nullptr when no suitable top level window exists.
QWidget*
mainWindowParent();
//=============================================================================
wstringVector
asTextList(const ArrayOf& value);
//=============================================================================
std::wstring
asText(const ArrayOf& value);
//=============================================================================
std::wstring
argText(const ArrayOfVector& args, size_t index, const std::wstring& defaultValue = L"");
//=============================================================================
bool
isName(const ArrayOf& value, const std::wstring& expected);
//=============================================================================
QString
nelsonIconPath();
//=============================================================================
ArrayOf
qObjectHandle(QObject* object, const QString& className);
//=============================================================================
double
numericElement(const ArrayOf& value, indexType index, double defaultValue);
//=============================================================================
QString
fileFilter(const ArrayOfVector& argIn);
//=============================================================================
QMessageBox::Icon
iconFromText(const std::wstring& icon);
//=============================================================================
FunctionDef*
findFunction(const std::string& name);
//=============================================================================
} // namespace Nelson::GuiDialogHelpers
//=============================================================================
