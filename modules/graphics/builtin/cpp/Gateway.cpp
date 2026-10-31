//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "OverloadName.hpp"
#include "GraphicsObjectClassNameResolver.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "__animatedline__Builtin.hpp"
#include "__annotation__Builtin.hpp"
#include "__axis_categorical__Builtin.hpp"
#include "__errorbar__Builtin.hpp"
#include "__functionline__Builtin.hpp"
#include "__line__Builtin.hpp"
#include "__stair__Builtin.hpp"
#include "__parameterizedfunctionline__Builtin.hpp"
#include "__scatter__Builtin.hpp"
#include "__subplottext__Builtin.hpp"
#include "__text__Builtin.hpp"
#include "__surf__Builtin.hpp"
#include "__fcontour__Builtin.hpp"
#include "__fimplicit__Builtin.hpp"
#include "__fimplicit3__Builtin.hpp"
#include "__fimplicit3_mesh__Builtin.hpp"
#include "__fsurf__Builtin.hpp"
#include "__isosurface__Builtin.hpp"
#include "__light__Builtin.hpp"
#include "__image__Builtin.hpp"
#include "__patch__Builtin.hpp"
#include "__rectangle__Builtin.hpp"
#include "__constantline__Builtin.hpp"
#include "__area__Builtin.hpp"
#include "__bar__Builtin.hpp"
#include "__binscatter__Builtin.hpp"
#include "__boxchart__Builtin.hpp"
#include "__bubblechart__Builtin.hpp"
#include "__bubblecloud__Builtin.hpp"
#include "__histogram__Builtin.hpp"
#include "__histogram2__Builtin.hpp"
#include "__piechart__Builtin.hpp"
#include "__donutchart__Builtin.hpp"
#include "__quiver__Builtin.hpp"
#include "__contour__Builtin.hpp"
#include "__contourc__Builtin.hpp"
#include "__compassplot__Builtin.hpp"
#include "__view__Builtin.hpp"
#include "__heatmapchart__Builtin.hpp"
#include "__parallelplot__Builtin.hpp"
#include "__scatterhistogram__Builtin.hpp"
#include "__hggroup__Builtin.hpp"
#include "__hgtransform__Builtin.hpp"
#include "__stackedplot__Builtin.hpp"
#include "__stackedplot_render__Builtin.hpp"
#include "__stem__Builtin.hpp"
#include "__streamslice2d__Builtin.hpp"
#include "__raincloudplot__Builtin.hpp"
#include "__violinplot__Builtin.hpp"
#include "__wordcloudchart__Builtin.hpp"
#include "__zoom__Builtin.hpp"
#include "__pan__Builtin.hpp"
#include "__default_interactivity__Builtin.hpp"
#include "__rotate3d__Builtin.hpp"
#include "__getframe__Builtin.hpp"
#include "__web_display_list__Builtin.hpp"
#include "__web_graphics_handle__Builtin.hpp"
#include "__web_figure_snapshot__Builtin.hpp"
#include "__web_figure_pick__Builtin.hpp"
#include "__web_figure_close__Builtin.hpp"
#include "__web_figure_key_event__Builtin.hpp"
#include "__web_figure_pan__Builtin.hpp"
#include "__web_figure_size__Builtin.hpp"
#include "__web_flush_callbacks__Builtin.hpp"
#include "__web_figure_reference__Builtin.hpp"
#include "__web_raster_png__Builtin.hpp"
#include "__web_uicontrol_action__Builtin.hpp"
#include "__figure_mouse_event__Builtin.hpp"
#include "__uicomponent_event__Builtin.hpp"
#include "__figure_native_geometry__Builtin.hpp"
#include "__uicontrol_key_event__Builtin.hpp"
#include "collapseBuiltin.hpp"
#include "expandBuiltin.hpp"
#include "uibuttonBuiltin.hpp"
#include "uibuttongroupBuiltin.hpp"
#include "uicheckboxBuiltin.hpp"
#include "uihtmlBuiltin.hpp"
#include "uitableBuiltin.hpp"
#include "uitreeBuiltin.hpp"
#include "uitreenodeBuiltin.hpp"
#include "uicontextmenuBuiltin.hpp"
#include "uicontrolBuiltin.hpp"
#include "uidatepickerBuiltin.hpp"
#include "uidropdownBuiltin.hpp"
#include "uieditfieldBuiltin.hpp"
#include "uifigureBuiltin.hpp"
#include "uigaugeBuiltin.hpp"
#include "uiknobBuiltin.hpp"
#include "uilampBuiltin.hpp"
#include "uisliderBuiltin.hpp"
#include "uiswitchBuiltin.hpp"
#include "uigridlayoutBuiltin.hpp"
#include "uilabelBuiltin.hpp"
#include "uilistboxBuiltin.hpp"
#include "uimenuBuiltin.hpp"
#include "uipanelBuiltin.hpp"
#include "uiradiobuttonBuiltin.hpp"
#include "uispinnerBuiltin.hpp"
#include "uitabBuiltin.hpp"
#include "uitabgroupBuiltin.hpp"
#include "uitextareaBuiltin.hpp"
#include "uitogglebuttonBuiltin.hpp"
#include "dialogBuiltin.hpp"
#include "msgboxBuiltin.hpp"
#include "errordlgBuiltin.hpp"
#include "helpdlgBuiltin.hpp"
#include "warndlgBuiltin.hpp"
#include "waitbarBuiltin.hpp"
#include "figureBuiltin.hpp"
#include "graphics_object_displayBuiltin.hpp"
#include "graphics_object_setBuiltin.hpp"
#include "graphics_object_getBuiltin.hpp"
#include "double_get_setBuiltin.hpp"
#include "graphics_object_fieldnamesBuiltin.hpp"
#include "graphics_object_isequalBuiltin.hpp"
#include "graphics_object_propertiesBuiltin.hpp"
#include "graphics_object_eqBuiltin.hpp"
#include "graphics_object_deleteBuiltin.hpp"
#include "isgraphicsBuiltin.hpp"
#include "grootBuiltin.hpp"
#include "isValidGraphicsPropertyBuiltin.hpp"
#include "canonicalGraphicsPropertyBuiltin.hpp"
#include "graphics_object_ispropBuiltin.hpp"
#include "graphics_object_ishandleBuiltin.hpp"
#include "CloseFigure.hpp"
#include "GOFiguresManager.hpp"
#include "axesBuiltin.hpp"
#include "gcfBuiltin.hpp"
#include "gcaBuiltin.hpp"
#include "gcoBuiltin.hpp"
#include "gcboBuiltin.hpp"
#include "gcbfBuiltin.hpp"
#include "gobjectsBuiltin.hpp"
#include "appdataBuiltins.hpp"
#include "closeBuiltin.hpp"
#include "colorbarBuiltin.hpp"
#include "legendBuiltin.hpp"
#include "bubblelegendBuiltin.hpp"
#include "is2DBuiltin.hpp"
#include "refreshBuiltin.hpp"
#include "drawnowBuiltin.hpp"
#include "validatecolorBuiltin.hpp"
#include "waitforbuttonpressBuiltin.hpp"
#include "waitforBuiltin.hpp"
#include "findobjBuiltin.hpp"
#include "findallBuiltin.hpp"
#include "findfigsBuiltin.hpp"
#include "allchildBuiltin.hpp"
#include "copyobjBuiltin.hpp"
#include "resetBuiltin.hpp"
#include "tiledlayoutBuiltin.hpp"
#include "nexttileBuiltin.hpp"
#include "tilenumBuiltin.hpp"
#include "tilerowcolBuiltin.hpp"
#include "addpointsBuiltin.hpp"
#include "clearpointsBuiltin.hpp"
#include "getpointsBuiltin.hpp"
#include "savefigBuiltin.hpp"
#include "openfigBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"graphics";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__animatedline__", (ptrBuiltin)Nelson::GraphicsGateway::__animatedline__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__annotation__", (ptrBuiltin)Nelson::GraphicsGateway::__annotation__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__axis_categorical__", (ptrBuiltin)Nelson::GraphicsGateway::__axis_categorical__Builtin, 0,
        3, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__scatter__", (ptrBuiltin)Nelson::GraphicsGateway::__scatter__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__line__", (ptrBuiltin)Nelson::GraphicsGateway::__line__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__stair__", (ptrBuiltin)Nelson::GraphicsGateway::__stair__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__functionline__", (ptrBuiltin)Nelson::GraphicsGateway::__functionline__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__parameterizedfunctionline__",
        (ptrBuiltin)Nelson::GraphicsGateway::__parameterizedfunctionline__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__errorbar__", (ptrBuiltin)Nelson::GraphicsGateway::__errorbar__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__text__", (ptrBuiltin)Nelson::GraphicsGateway::__text__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__subplottext__", (ptrBuiltin)Nelson::GraphicsGateway::__subplottext__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__surf__", (ptrBuiltin)Nelson::GraphicsGateway::__surf__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__fsurf__", (ptrBuiltin)Nelson::GraphicsGateway::__fsurf__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__fcontour__", (ptrBuiltin)Nelson::GraphicsGateway::__fcontour__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__fimplicit__", (ptrBuiltin)Nelson::GraphicsGateway::__fimplicit__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__fimplicit3__", (ptrBuiltin)Nelson::GraphicsGateway::__fimplicit3__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__fimplicit3_mesh__", (ptrBuiltin)Nelson::GraphicsGateway::__fimplicit3_mesh__Builtin, 2, 3,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "__isosurface__", (ptrBuiltin)Nelson::GraphicsGateway::__isosurface__Builtin, 2, 7,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__light__", (ptrBuiltin)Nelson::GraphicsGateway::__light__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__image__", (ptrBuiltin)Nelson::GraphicsGateway::__image__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__heatmapchart__", (ptrBuiltin)Nelson::GraphicsGateway::__heatmapchart__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__parallelplot__", (ptrBuiltin)Nelson::GraphicsGateway::__parallelplot__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__scatterhistogram__", (ptrBuiltin)Nelson::GraphicsGateway::__scatterhistogram__Builtin, -1,
        1, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__stem__", (ptrBuiltin)Nelson::GraphicsGateway::__stem__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__streamslice2d__", (ptrBuiltin)Nelson::GraphicsGateway::__streamslice2d__Builtin, 2, 6,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__raincloudplot__", (ptrBuiltin)Nelson::GraphicsGateway::__raincloudplot__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__violinplot__", (ptrBuiltin)Nelson::GraphicsGateway::__violinplot__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__wordcloudchart__", (ptrBuiltin)Nelson::GraphicsGateway::__wordcloudchart__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__patch__", (ptrBuiltin)Nelson::GraphicsGateway::__patch__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__rectangle__", (ptrBuiltin)Nelson::GraphicsGateway::__rectangle__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__constantline__", (ptrBuiltin)Nelson::GraphicsGateway::__constantline__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__area__", (ptrBuiltin)Nelson::GraphicsGateway::__area__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__bar__", (ptrBuiltin)Nelson::GraphicsGateway::__bar__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__binscatter__", (ptrBuiltin)Nelson::GraphicsGateway::__binscatter__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__boxchart__", (ptrBuiltin)Nelson::GraphicsGateway::__boxchart__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__bubblechart__", (ptrBuiltin)Nelson::GraphicsGateway::__bubblechart__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__bubblecloud__", (ptrBuiltin)Nelson::GraphicsGateway::__bubblecloud__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__histogram__", (ptrBuiltin)Nelson::GraphicsGateway::__histogram__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__histogram2__", (ptrBuiltin)Nelson::GraphicsGateway::__histogram2__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__piechart__", (ptrBuiltin)Nelson::GraphicsGateway::__piechart__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__donutchart__", (ptrBuiltin)Nelson::GraphicsGateway::__donutchart__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__quiver__", (ptrBuiltin)Nelson::GraphicsGateway::__quiver__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__contour__", (ptrBuiltin)Nelson::GraphicsGateway::__contour__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__contourc__", (ptrBuiltin)Nelson::GraphicsGateway::__contourc__Builtin, 1, 4, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__compassplot__", (ptrBuiltin)Nelson::GraphicsGateway::__compassplot__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__hggroup__", (ptrBuiltin)Nelson::GraphicsGateway::__hggroup__Builtin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__hgtransform__", (ptrBuiltin)Nelson::GraphicsGateway::__hgtransform__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__stackedplot__", (ptrBuiltin)Nelson::GraphicsGateway::__stackedplot__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__stackedplot_render__", (ptrBuiltin)Nelson::GraphicsGateway::__stackedplot_render__Builtin,
        -1, 3, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__view__", (ptrBuiltin)Nelson::GraphicsGateway::__view__Builtin, -1, 4, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__zoom__", (ptrBuiltin)Nelson::GraphicsGateway::__zoom__Builtin, -1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__pan__", (ptrBuiltin)Nelson::GraphicsGateway::__pan__Builtin, -1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__default_interactivity__",
        (ptrBuiltin)Nelson::GraphicsGateway::__default_interactivity__Builtin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__rotate3d__", (ptrBuiltin)Nelson::GraphicsGateway::__rotate3d__Builtin, -1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__getframe__", (ptrBuiltin)Nelson::GraphicsGateway::__getframe__Builtin, -1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__web_display_list__", (ptrBuiltin)Nelson::GraphicsGateway::__web_display_list__Builtin, -1,
        6, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_state__", (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_state__Builtin, 1,
        1, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_snapshot__",
        (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_snapshot__Builtin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_history_data__",
        (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_history_data__Builtin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__web_graphics_handle__",
        (ptrBuiltin)Nelson::GraphicsGateway::__web_graphics_handle__Builtin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_pick__", (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_pick__Builtin, 1, 5,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_close__", (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_close__Builtin, 1,
        1, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_key_event__",
        (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_key_event__Builtin, 1, 8, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_pan__", (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_pan__Builtin, 1, 3,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_reference__",
        (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_reference__Builtin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__web_figure_size__", (ptrBuiltin)Nelson::GraphicsGateway::__web_figure_size__Builtin, 1, 3,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__web_flush_callbacks__",
        (ptrBuiltin)Nelson::GraphicsGateway::__web_flush_callbacks__Builtin, 1, 0, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__web_raster_png__", (ptrBuiltin)Nelson::GraphicsGateway::__web_raster_png__Builtin, -1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__web_uicontrol_action__",
        (ptrBuiltin)Nelson::GraphicsGateway::__web_uicontrol_action__Builtin, 1, 3, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__figure_mouse_event__", (ptrBuiltin)Nelson::GraphicsGateway::__figure_mouse_event__Builtin,
        1, 4, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__uicomponent_event__", (ptrBuiltin)Nelson::GraphicsGateway::__uicomponent_event__Builtin, 1,
        4, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "__figure_native_geometry__",
        (ptrBuiltin)Nelson::GraphicsGateway::__figure_native_geometry__Builtin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__uicontrol_key_event__",
        (ptrBuiltin)Nelson::GraphicsGateway::__uicontrol_key_event__Builtin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "display"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "disp"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "properties"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_propertiesBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "fieldnames"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "delete"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "set"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_setBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "get"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_getBuiltin, -1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    // Graphics handles can be passed as plain numbers (0 is the root), so
    // get/set must accept doubles as well as graphics_object values.
    { OVERLOAD_FUNCTION_NAME(NLS_DOUBLE_STR, "get"),
        (ptrBuiltin)Nelson::GraphicsGateway::double_getBuiltin, -1, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_DOUBLE_STR, "set"),
        (ptrBuiltin)Nelson::GraphicsGateway::double_setBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isequal"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isequaln"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isequalto"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isprop"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "ishandle"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_ishandleBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isvalid"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_ishandleBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "eq"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_eqBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "isgraphics", (ptrBuiltin)Nelson::GraphicsGateway::isgraphicsBuiltin, 1, 1, CPP_BUILTIN },
    { "groot", (ptrBuiltin)Nelson::GraphicsGateway::grootBuiltin, 1, 0, CPP_BUILTIN },
    { "gobjects", (ptrBuiltin)Nelson::GraphicsGateway::gobjectsBuiltin, 1, -1, CPP_BUILTIN },
    { "setappdata", (ptrBuiltin)Nelson::GraphicsGateway::setappdataBuiltin, 0, 3, CPP_BUILTIN },
    { "getappdata", (ptrBuiltin)Nelson::GraphicsGateway::getappdataBuiltin, 1, -1, CPP_BUILTIN },
    { "isappdata", (ptrBuiltin)Nelson::GraphicsGateway::isappdataBuiltin, 1, 2, CPP_BUILTIN },
    { "rmappdata", (ptrBuiltin)Nelson::GraphicsGateway::rmappdataBuiltin, 0, 2, CPP_BUILTIN },
    { "isValidGraphicsProperty",
        (ptrBuiltin)Nelson::GraphicsGateway::isValidGraphicsPropertyBuiltin, 1, 2, CPP_BUILTIN },
    { "canonicalGraphicsProperty",
        (ptrBuiltin)Nelson::GraphicsGateway::canonicalGraphicsPropertyBuiltin, 1, 2, CPP_BUILTIN },
    { "axes", (ptrBuiltin)Nelson::GraphicsGateway::axesBuiltin, -1, 1, CPP_BUILTIN },
    { "__polaraxes__", (ptrBuiltin)Nelson::GraphicsGateway::polaraxesBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__uiaxes__", (ptrBuiltin)Nelson::GraphicsGateway::uiaxesBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "__yyaxis__", (ptrBuiltin)Nelson::GraphicsGateway::yyaxisBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "colorbar", (ptrBuiltin)Nelson::GraphicsGateway::colorbarBuiltin, -1, 1, CPP_BUILTIN },
    { "legend", (ptrBuiltin)Nelson::GraphicsGateway::legendBuiltin, -1, 4, CPP_BUILTIN },
    { "bubblelegend", (ptrBuiltin)Nelson::GraphicsGateway::bubblelegendBuiltin, -1, 1,
        CPP_BUILTIN },
    { "gcf", (ptrBuiltin)Nelson::GraphicsGateway::gcfBuiltin, -1, 0, CPP_BUILTIN },
    { "gca", (ptrBuiltin)Nelson::GraphicsGateway::gcaBuiltin, -1, 0, CPP_BUILTIN },
    { "gco", (ptrBuiltin)Nelson::GraphicsGateway::gcoBuiltin, 1, 0, CPP_BUILTIN },
    { "gcbo", (ptrBuiltin)Nelson::GraphicsGateway::gcboBuiltin, 2, 0, CPP_BUILTIN },
    { "gcbf", (ptrBuiltin)Nelson::GraphicsGateway::gcbfBuiltin, 1, 0, CPP_BUILTIN },
    { "close", (ptrBuiltin)Nelson::GraphicsGateway::closeBuiltin, -1, 0, CPP_BUILTIN },
    { "is2D", (ptrBuiltin)Nelson::GraphicsGateway::is2DBuiltin, 1, 1, CPP_BUILTIN },
    { "refresh", (ptrBuiltin)Nelson::GraphicsGateway::refreshBuiltin, 0, 1, CPP_BUILTIN },
    { "drawnow", (ptrBuiltin)Nelson::GraphicsGateway::drawnowBuiltin, 0, 1, CPP_BUILTIN },
    { "validatecolor", (ptrBuiltin)Nelson::GraphicsGateway::validatecolorBuiltin, 1, 2,
        CPP_BUILTIN },
    { "figure", (ptrBuiltin)Nelson::GraphicsGateway::figureBuiltin, 1, 1, CPP_BUILTIN },
    { "dialog", (ptrBuiltin)Nelson::GraphicsGateway::dialogBuiltin, 1, -1, CPP_BUILTIN },
    { "msgbox", (ptrBuiltin)Nelson::GraphicsGateway::msgboxBuiltin, -1, -1, CPP_BUILTIN },
    { "helpdlg", (ptrBuiltin)Nelson::GraphicsGateway::helpdlgBuiltin, -1, 2, CPP_BUILTIN },
    { "warndlg", (ptrBuiltin)Nelson::GraphicsGateway::warndlgBuiltin, -1, 3, CPP_BUILTIN },
    { "errordlg", (ptrBuiltin)Nelson::GraphicsGateway::errordlgBuiltin, -1, 3, CPP_BUILTIN },
    { "waitbar", (ptrBuiltin)Nelson::GraphicsGateway::waitbarBuiltin, 1, -3, CPP_BUILTIN },
    { "uicontextmenu", (ptrBuiltin)Nelson::GraphicsGateway::uicontextmenuBuiltin, -1, 1,
        CPP_BUILTIN },
    { "uibutton", (ptrBuiltin)Nelson::GraphicsGateway::uibuttonBuiltin, -1, -1, CPP_BUILTIN },
    { "uibuttongroup", (ptrBuiltin)Nelson::GraphicsGateway::uibuttongroupBuiltin, -1, -1,
        CPP_BUILTIN },
    { "uicheckbox", (ptrBuiltin)Nelson::GraphicsGateway::uicheckboxBuiltin, -1, -1, CPP_BUILTIN },
    { "uicontrol", (ptrBuiltin)Nelson::GraphicsGateway::uicontrolBuiltin, -1, -1, CPP_BUILTIN },
    { "uidatepicker", (ptrBuiltin)Nelson::GraphicsGateway::uidatepickerBuiltin, -1, -1,
        CPP_BUILTIN },
    { "uidropdown", (ptrBuiltin)Nelson::GraphicsGateway::uidropdownBuiltin, -1, -1, CPP_BUILTIN },
    { "uigauge", (ptrBuiltin)Nelson::GraphicsGateway::uigaugeBuiltin, -1, -1, CPP_BUILTIN },
    { "uiknob", (ptrBuiltin)Nelson::GraphicsGateway::uiknobBuiltin, -1, -1, CPP_BUILTIN },
    { "uilamp", (ptrBuiltin)Nelson::GraphicsGateway::uilampBuiltin, -1, -1, CPP_BUILTIN },
    { "uislider", (ptrBuiltin)Nelson::GraphicsGateway::uisliderBuiltin, -1, -1, CPP_BUILTIN },
    { "uiswitch", (ptrBuiltin)Nelson::GraphicsGateway::uiswitchBuiltin, -1, -1, CPP_BUILTIN },
    { "uieditfield", (ptrBuiltin)Nelson::GraphicsGateway::uieditfieldBuiltin, -1, -1, CPP_BUILTIN },
    { "uilistbox", (ptrBuiltin)Nelson::GraphicsGateway::uilistboxBuiltin, -1, -1, CPP_BUILTIN },
    { "uiradiobutton", (ptrBuiltin)Nelson::GraphicsGateway::uiradiobuttonBuiltin, -1, -1,
        CPP_BUILTIN },
    { "uispinner", (ptrBuiltin)Nelson::GraphicsGateway::uispinnerBuiltin, -1, -1, CPP_BUILTIN },
    { "uihtml", (ptrBuiltin)Nelson::GraphicsGateway::uihtmlBuiltin, -1, -1, CPP_BUILTIN },
    { "sendEventToHTMLSource", (ptrBuiltin)Nelson::GraphicsGateway::sendeventtohtmlsourceBuiltin,
        -1, -1, CPP_BUILTIN },
    { "uitable", (ptrBuiltin)Nelson::GraphicsGateway::uitableBuiltin, -1, -1, CPP_BUILTIN },
    { "uitextarea", (ptrBuiltin)Nelson::GraphicsGateway::uitextareaBuiltin, -1, -1, CPP_BUILTIN },
    { "uitogglebutton", (ptrBuiltin)Nelson::GraphicsGateway::uitogglebuttonBuiltin, -1, -1,
        CPP_BUILTIN },
    { "uitree", (ptrBuiltin)Nelson::GraphicsGateway::uitreeBuiltin, -1, -1, CPP_BUILTIN },
    { "uitreenode", (ptrBuiltin)Nelson::GraphicsGateway::uitreenodeBuiltin, -1, -1, CPP_BUILTIN },
    { "expand", (ptrBuiltin)Nelson::GraphicsGateway::expandBuiltin, 0, 2, CPP_BUILTIN },
    { "collapse", (ptrBuiltin)Nelson::GraphicsGateway::collapseBuiltin, 0, 2, CPP_BUILTIN },
    { "uifigure", (ptrBuiltin)Nelson::GraphicsGateway::uifigureBuiltin, 1, -1, CPP_BUILTIN },
    { "uigridlayout", (ptrBuiltin)Nelson::GraphicsGateway::uigridlayoutBuiltin, -1, -1,
        CPP_BUILTIN },
    { "uilabel", (ptrBuiltin)Nelson::GraphicsGateway::uilabelBuiltin, -1, -1, CPP_BUILTIN },
    { "uipanel", (ptrBuiltin)Nelson::GraphicsGateway::uipanelBuiltin, -1, -1, CPP_BUILTIN },
    { "uitab", (ptrBuiltin)Nelson::GraphicsGateway::uitabBuiltin, -1, -1, CPP_BUILTIN },
    { "uitabgroup", (ptrBuiltin)Nelson::GraphicsGateway::uitabgroupBuiltin, -1, -1, CPP_BUILTIN },
    { "uimenu", (ptrBuiltin)Nelson::GraphicsGateway::uimenuBuiltin, -1, 1, CPP_BUILTIN },
    { "waitforbuttonpress", (ptrBuiltin)Nelson::GraphicsGateway::waitforbuttonpressBuiltin, -1, 0,
        CPP_BUILTIN },
    { "waitfor", (ptrBuiltin)Nelson::GraphicsGateway::waitforBuiltin, 0, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "findobj", (ptrBuiltin)Nelson::GraphicsGateway::findobjBuiltin, 1, -1, CPP_BUILTIN },
    { "findall", (ptrBuiltin)Nelson::GraphicsGateway::findallBuiltin, 1, -1, CPP_BUILTIN },
    { "findfigs", (ptrBuiltin)Nelson::GraphicsGateway::findfigsBuiltin, 1, 0, CPP_BUILTIN },
    { "allchild", (ptrBuiltin)Nelson::GraphicsGateway::allchildBuiltin, 1, 1, CPP_BUILTIN },
    { "copyobj", (ptrBuiltin)Nelson::GraphicsGateway::copyobjBuiltin, 1, 2, CPP_BUILTIN },
    { "reset", (ptrBuiltin)Nelson::GraphicsGateway::resetBuiltin, 0, 1, CPP_BUILTIN },
    { "tiledlayout", (ptrBuiltin)Nelson::GraphicsGateway::tiledlayoutBuiltin, 1, 0, CPP_BUILTIN },
    { "nexttile", (ptrBuiltin)Nelson::GraphicsGateway::nexttileBuiltin, 1, 0, CPP_BUILTIN },
    { "tilenum", (ptrBuiltin)Nelson::GraphicsGateway::tilenumBuiltin, 1, 1, CPP_BUILTIN },
    { "tilerowcol", (ptrBuiltin)Nelson::GraphicsGateway::tilerowcolBuiltin, 2, 1, CPP_BUILTIN },
    { "addpoints", (ptrBuiltin)Nelson::GraphicsGateway::addpointsBuiltin, 0, -1, CPP_BUILTIN },
    { "clearpoints", (ptrBuiltin)Nelson::GraphicsGateway::clearpointsBuiltin, 0, 1, CPP_BUILTIN },
    { "getpoints", (ptrBuiltin)Nelson::GraphicsGateway::getpointsBuiltin, 3, 1, CPP_BUILTIN },
    { "__savefig__", (ptrBuiltin)Nelson::GraphicsGateway::savefigBuiltin, 1, 1, CPP_BUILTIN },
    { "__openfig__", (ptrBuiltin)Nelson::GraphicsGateway::openfigBuiltin, 1, 4, CPP_BUILTIN },
};
//=============================================================================
static std::string
graphicsObjectClassNameFromHandle(go_handle handle)
{
    if (isUnassignedGraphicsObject(handle)) {
        return GO_CLASS_NAME_GRAPHICS_PLACEHOLDER_STR;
    }
    GraphicsObject* object = graphicsObjectFromHandle(handle, false);
    return object == nullptr ? std::string() : object->getClassName();
}
//=============================================================================
static bool
initializeGraphicsModule(Nelson::Evaluator* eval)
{
    setGraphicsObjectClassNameResolver(graphicsObjectClassNameFromHandle);
    initializeGraphic();
    return true;
}
//=============================================================================
static bool
finishGraphicsModule(Nelson::Evaluator* eval)
{
    closeAllFigures();
    shutdownGraphic();
    setGraphicsObjectClassNameResolver(nullptr);
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED_LAZY(gateway, (void*)initializeGraphicsModule)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishGraphicsModule)
//=============================================================================
