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
#include <functional>
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "nlsGraphics_exports.h"
#include "UIParentRules.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIComponent;
//=============================================================================
// factory creating the graphics object for a resolved style
// (empty style when the component has no style argument)
using UIComponentFactory = std::function<GOUIComponent*(const std::wstring& styleName)>;
//=============================================================================
struct UIComponentBuilderOptions
{
    wstringVector allowedStyles;
    std::wstring defaultStyle;
    UIParentKind parentKind = UIParentKind::AnyContainer;
    bool acceptsSizeArgument = false; // uigridlayout(parent, [rows cols])
};
//=============================================================================
// Generic builder for app-style UI components:
//   h = uixxx()
//   h = uixxx(parent)
//   h = uixxx(style)
//   h = uixxx(parent, style)
//   h = uixxx(___, Name, Value, ...)
// Creates a new uifigure-styled figure when no parent is provided.
NLSGRAPHICS_IMPEXP go_handle
UIComponentBuilder(const ArrayOfVector& argIn, const UIComponentBuilderOptions& options,
    const UIComponentFactory& factory);
//=============================================================================
// per-component builders used by the builtin layer
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIButtonBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UILabelBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIPanelBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIButtonGroupBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UITabGroupBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UITabBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIGridLayoutBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UICheckBoxBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIRadioButtonBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIToggleButtonBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIDropDownBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIListBoxBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIEditFieldBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UITextAreaBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UISpinnerBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UISliderBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIKnobBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIGaugeBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UILampBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UISwitchBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIDatePickerBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UITableBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UIHtmlBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UITreeBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
UITreeNodeBuilder(const ArrayOfVector& argIn);
//=============================================================================
// expand/collapse the tree owning the given tree or tree node handle
NLSGRAPHICS_IMPEXP void
UITreeExpand(go_handle handle, bool expand);
//=============================================================================
} // namespace Nelson
//=============================================================================
