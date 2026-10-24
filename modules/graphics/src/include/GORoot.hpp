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
#include <unordered_map>
#include <unordered_set>
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GORoot : public GraphicsObject
{
private:
    std::unordered_map<std::wstring, ArrayOf> m_defaultPropertyOverrides;
    std::unordered_map<std::wstring, ArrayOf> m_factoryDefaultProperties;
    std::unordered_set<std::wstring> m_factoryDefaultsRememberedTypes;
    bool m_collectingFactoryDefaults = false;
    bool m_recordFactoryDefaultsNow = false;

    void
    ensureFactoryDefaultsForType(const std::wstring& objectType);

    void
    refreshCurrentFigureProperty();
    void
    refreshCallbackObjectProperty();
    void
    refreshChildrenProperty();
    void
    refreshMonitorPositionsProperty();
    void
    refreshScreenSizeProperty();
    void
    refreshScreenDepthProperty();
    void
    refreshScreenPixelsPerInchProperty();
    void
    refreshCommandWindowSizeProperty();
    void
    refreshFormatProperties();

public:
    GORoot();
    ~GORoot() override = default;
    virtual std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_ROOT_STR;
    }
    virtual void
    constructProperties();
    void
    updateState() override;
    void
    setupDefaults();
    void
    loadDefaultColorMap();
    void
    loadDefaultAlphaMap();
    bool
    isDefaultPropertyName(const std::wstring& name) const;
    bool
    getDefaultProperty(const std::wstring& name, ArrayOf& value) const;
    void
    setDefaultProperty(const std::wstring& name, const ArrayOf& value);
    void
    removeDefaultProperty(const std::wstring& name);
    void
    rememberFactoryDefaults(GraphicsObject* go);
    void
    applyDefaultProperties(GraphicsObject* go);
    void
    setCurrentFigureProperty(const ArrayOf& value);
    void
    resetProperties();
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
NLSGRAPHICS_IMPEXP int64
graphicsRootObject();
//=============================================================================
NLSGRAPHICS_IMPEXP GORoot*
getGraphicsRootObject();
//=============================================================================
NLSGRAPHICS_IMPEXP std::wstring
resolveRootFontName(const std::wstring& fontName);
//=============================================================================
} // namespace Nelson
//=============================================================================
