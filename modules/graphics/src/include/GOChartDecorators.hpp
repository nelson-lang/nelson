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
#include <string>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Decorator objects that a chart exposes through a typed property (BaseLine, Annotation,
// DataTipTemplate, LegendInformation). They never paint: the owning chart draws its own
// baseline and the legend/datatip code reads them as plain option carriers. They only
// answer get()/set() and report the class name.
//=============================================================================
class NLSGRAPHICS_IMPEXP GOBaseline : public GraphicsObject
{
public:
    GOBaseline();
    ~GOBaseline() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override;
    void
    registerProperties() override;
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOLegendEntry : public GraphicsObject
{
public:
    GOLegendEntry();
    ~GOLegendEntry() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override;
    void
    registerProperties() override;
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOAnnotationEventData : public GraphicsObject
{
public:
    GOAnnotationEventData();
    ~GOAnnotationEventData() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override;
    void
    registerProperties() override;
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GODataTipTemplate : public GraphicsObject
{
public:
    GODataTipTemplate();
    ~GODataTipTemplate() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override;
    void
    registerProperties() override;
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
enum class GODecoratorKind
{
    Baseline,
    AnnotationEventData,
    LegendEntry,
    DataTipTemplate
};
//=============================================================================
// Builds its decorator on first get(). The decorator is a registered graphics object, so
// class(get(chart).BaseLine) resolves to its class name and size() reports [1 1].
//=============================================================================
class NLSGRAPHICS_IMPEXP GOLazyDecoratorProperty : public GOGObjectsProperty
{
    GODecoratorKind m_kind;

public:
    explicit GOLazyDecoratorProperty(GODecoratorKind kind);
    ~GOLazyDecoratorProperty() override = default;
    ArrayOf
    get() override;
};
//=============================================================================
// While empty, reports a typed 0x0 placeholder instead of the generic GraphicsPlaceholder.
// Used for Layout, until a tiled layout populates it.
//=============================================================================
class NLSGRAPHICS_IMPEXP GOLayoutOptionsProperty : public GOGObjectsProperty
{
public:
    GOLayoutOptionsProperty();
    ~GOLayoutOptionsProperty() override = default;
    ArrayOf
    get() override;
};
//=============================================================================
// While unset, reports an empty 0x0 table instead of an empty double. Used for SourceTable
// until a chart is built from a table.
//=============================================================================
class NLSGRAPHICS_IMPEXP GOSourceTableProperty : public GOArrayOfProperty
{
    // stackedplot reports an empty timetable, every other chart an empty table.
    bool m_asTimetable;

public:
    explicit GOSourceTableProperty(bool asTimetable = false);
    ~GOSourceTableProperty() override = default;
    ArrayOf
    get() override;
};
//=============================================================================
// While unset, reports an empty 0x3 table with the Target / Style / TargetIndex variables.
// Used for StyleConfigurations on uitable, uidropdown and uitree until addStyle populates it.
//=============================================================================
class NLSGRAPHICS_IMPEXP GOStyleConfigurationsProperty : public GOArrayOfProperty
{
public:
    GOStyleConfigurationsProperty() = default;
    ~GOStyleConfigurationsProperty() override = default;
    ArrayOf
    get() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
