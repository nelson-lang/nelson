#include <math.h>
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOFigure.hpp"
#include "GOList.hpp"
#include "GOWindow.hpp"
#include "GONextPlotModeProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOColorVectorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "GOFiguresManager.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOToolBarProperty.hpp"
#include "GOMenuBarProperty.hpp"
#include "BaseFigureQt.hpp"
#include "NelsonConfiguration.hpp"
#include "GOCallbackProperty.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "ProcessEvents.hpp"
#include "CallbackQueue.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOWindowStateProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOFigure::getType()
{
    return GO_PROPERTY_VALUE_FIGURE_STR;
}
//=============================================================================
GOFigure::GOFigure(GOWindow* win, int number)
{
    m_width = 560;
    m_height = 445;
    m_win = win;
    registerProperties();
    initializeProperties();
    setScalarDoubleDefault(GO_NUMBER_PROPERTY_NAME_STR, (double)number);
}
//=============================================================================
GOWindow*
GOFigure::getGOWindow()
{
    return m_win;
}
//=============================================================================
void
GOFigure::registerProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_NUMBER_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_ALPHA_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorVectorProperty, GO_COLOR_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CURRENT_AXES_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR, false);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_NAME_PROPERTY_NAME_STR);
    registerProperty(new GONextPlotModeProperty, GO_NEXT_PLOT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_DRAW_LATER_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_NUMBER_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_GRAPHICS_SMOOTHING_PROPERTY_NAME_STR);
    registerProperty(new GOToolBarProperty, GO_TOOL_BAR_PROPERTY_NAME_STR);
    registerProperty(new GOMenuBarProperty, GO_MENU_BAR_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CLOSE_REQUEST_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_RESIZE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, false);
    registerProperty(new GOWindowStateProperty, GO_WINDOW_STATE_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOFigure::initializeProperties()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.94, 0.94, 0.94);
    setStringDefault(GO_NEXT_PLOT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REPLACE_STR);
    setGoProperty(GO_PARENT_PROPERTY_NAME_STR, graphicsRootObject());
    setStringDefault(GO_NAME_PROPERTY_NAME_STR, {});
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 488, 242, 560, 420);
    setRestrictedStringDefault(GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_GRAPHICS_SMOOTHING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_TOOL_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MENU_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FIGURE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_WINDOW_STATE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_RESIZE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, 1.0);
    GOGenericProperty* hp = findProperty(GO_CLOSE_REQUEST_FCN_PROPERTY_NAME_STR);
    if (hp) {
        hp->set(ArrayOf::characterArrayConstructor(GO_PROPERTY_VALUE_CLOSEREQ_STR));
    }
    hp = findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    if (hp) {
        hp->set(ArrayOf::characterArrayConstructor(L""));
    }
    loadDefaultAlphaMap();
    loadDefaultColorMap();
    _resized = false;
}
//=============================================================================
void
GOFigure::setFocus()
{
    GOScalarProperty* hp = (GOScalarProperty*)findProperty(GO_NUMBER_PROPERTY_NAME_STR);
    int figureNumber = (int)hp->data();
    if (NelsonConfiguration::getInstance()->isCurrentFigureOnClick()) {
        notifyCurrentFigureChanged(figureNumber - 1);
    }
}
//=============================================================================
void
GOFigure::repaint()
{
    m_win->update();
}
//=============================================================================
void
HSVRAMP(double h, double& r, double& g, double& b)
{
    int i;
    double f, p, q, t;
    h *= 6;
    i = (int)floor(h);
    f = h - i;
    p = 0;
    q = 1 - f;
    t = f;
    switch (i) {
    case 0:
        r = 1;
        g = t;
        b = p;
        break;
    case 1:
        r = q;
        g = 1;
        b = p;
        break;
    case 2:
        r = p;
        g = 1;
        b = t;
        break;
    case 3:
        r = p;
        g = q;
        b = 1;
        break;
    case 4:
        r = t;
        g = p;
        b = 1;
        break;
    default:
        r = 1;
        g = p;
        b = q;
        break;
    }
}
//=============================================================================
void
GOFigure::updateState(bool forceUpdate)
{
    m_win->updateState(forceUpdate);
    if (hasChanged(GO_COLOR_MAP_PROPERTY_NAME_STR) || forceUpdate) {
        GOGObjectsProperty* children
            = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        std::vector<int64> handles(children->data());
        ArrayOf colormapAsArrayOf = findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)->get();
        for (size_t i = 0; i < handles.size(); i++) {
            GraphicsObject* fp = findGraphicsObject(handles[i]);
            if (fp->haveProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)) {
                fp->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)->set(colormapAsArrayOf);
                fp->updateState();
            }
        }
        clearChanged(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    refreshPositionProperties(forceUpdate);
    refreshDrawLaterProperty(forceUpdate);
}
//=============================================================================
void
GOFigure::updateState()
{
    m_win->updateState(false);
    this->updateState(false);
}
//=============================================================================
void
GOFigure::loadDefaultAlphaMap()
{
    GORoot* groot = getGraphicsRootObject();
    if (groot) {
        GOVectorProperty* hvDefault
            = (GOVectorProperty*)groot->findProperty(GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR);
        GOVectorProperty* hv = (GOVectorProperty*)findProperty(GO_ALPHA_MAP_PROPERTY_NAME_STR);
        hv->data(hvDefault->data());
    }
}
//=============================================================================
void
GOFigure::loadDefaultColorMap()
{
    GORoot* groot = getGraphicsRootObject();
    if (groot) {
        GOVectorProperty* hvDefault
            = (GOVectorProperty*)groot->findProperty(GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR);
        GOColorVectorProperty* hv
            = (GOColorVectorProperty*)findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
        hv->data(hvDefault->data());
    }
}
//=============================================================================
void
GOFigure::paintMe(RenderInterface& gc)
{
    GOOnOffProperty* drawLaterProperty
        = static_cast<GOOnOffProperty*>(findProperty(GO_DRAW_LATER_PROPERTY_NAME_STR));
    if (!drawLaterProperty->asBool()) {
        GOOnOffProperty* graphicsSmoothingProperty
            = static_cast<GOOnOffProperty*>(findProperty(GO_GRAPHICS_SMOOTHING_PROPERTY_NAME_STR));
        gc.setGraphicsSmoothing(graphicsSmoothingProperty->asBool());
        GOColorProperty* color
            = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
        gc.clear(color->data());
        GOGObjectsProperty* children
            = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        GOGObjectsProperty* currentAxes
            = static_cast<GOGObjectsProperty*>(findProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR));
        std::vector<int64> handlesCurrentAxes(currentAxes->data());
        std::vector<int64> handlesChildren(children->data());
        std::vector<GraphicsObject*> legendObjects;
        std::vector<GraphicsObject*> uiControlObjects;

        for (ompIndexType i = 0; i < (ompIndexType)handlesChildren.size(); i++) {
            if (handlesCurrentAxes.empty()) {
                GraphicsObject* fp = findGraphicsObject(handlesChildren[i], false);
                if (fp) {
                    fp->paintMe(gc);
                }
            } else if (handlesChildren[i] != handlesCurrentAxes[0]) {
                GraphicsObject* fp = findGraphicsObject(handlesChildren[i], false);
                if (fp) {
                    if (fp->getType() == GO_PROPERTY_VALUE_UICONTROL_STR) {
                        uiControlObjects.push_back(fp);
                    } else {
                        fp->paintMe(gc);
                        GOStringProperty* ft = static_cast<GOStringProperty*>(
                            fp->findProperty(GO_TAG_PROPERTY_NAME_STR, false));
                        GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                            fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
                        if (ft && fv) {
                            if (ft->data() == L"legend" && fv->data() == GO_PROPERTY_VALUE_ON_STR) {
                                legendObjects.push_back(fp);
                            }
                        }
                    }
                }
            }
        }
        if (!handlesCurrentAxes.empty()) {
            GraphicsObject* fp = findGraphicsObject(handlesCurrentAxes[0], false);
            if (fp) {
                fp->paintMe(gc);
            }
        }
        if (!legendObjects.empty()) {
            for (auto l : legendObjects) {
                if (l) {
                    l->paintMe(gc);
                }
            }
        }
        if (!uiControlObjects.empty()) {
            for (auto uio : uiControlObjects) {
                if (uio) {
                    uio->paintMe(gc);
                }
            }
        }

        _resized = false;
        setRenderingStateInvalid(false);
    }
}
//=============================================================================
void
GOFigure::resizeGL(int width, int height)
{
    if ((m_width == width) && (m_height == height)) {
        return;
    }
    _resized = true;
    QRect qGeometry = m_win->frameGeometry();
    QPoint qPoint = qGeometry.topLeft();
    m_width = width;
    m_height = height;

    int transformedY
        = transformY(qPoint.y(), height, Nelson::BaseFigureQt::getCurrentScreenHeight());

    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, qPoint.x(), transformedY, width, height);
    updateState();
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    for (ompIndexType i = 0; i < (ompIndexType)handles.size(); i++) {
        GraphicsObject* fp = findGraphicsObject(handles[i]);
        if (fp) {
            if (fp->haveProperty(GO_POSITION_PROPERTY_NAME_STR)) {
                GOGenericProperty* gop = fp->findProperty(GO_POSITION_PROPERTY_NAME_STR);
                if (gop) {
                    gop->setModified(true);
                }
            }
            fp->updateState();
        }
    }
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->pushEvent(this, L"SizeChangedData", L"SizeChanged");
    }
}
//=============================================================================
bool
GOFigure::resized()
{
    return _resized;
}
//=============================================================================
int
GOFigure::transformY(int y, int heightFrame, int screenHeight)
{
    int transformedY = screenHeight - heightFrame - y;
    return transformedY;
}
//=============================================================================
void
GOFigure::refreshPositionProperties(bool forceUpdate)
{
    GOFourVectorProperty* property
        = static_cast<GOFourVectorProperty*>(findProperty(GO_POSITION_PROPERTY_NAME_STR));
    QRect qFrameGeometry = m_win->frameGeometry();
    QSize qSize = qFrameGeometry.size();
    QPoint qPoint = qFrameGeometry.topLeft();

    auto* positionProperty = static_cast<GOFourVectorProperty*>(property);
    int transformedY
        = transformY(qPoint.y(), m_height, Nelson::BaseFigureQt::getCurrentScreenHeight());
    positionProperty->value(qPoint.x(), transformedY, qSize.width(), qSize.height());
};
//=============================================================================
void
GOFigure::refreshDrawLaterProperty(bool forceUpdate)
{
    if (hasChanged(GO_DRAW_LATER_PROPERTY_NAME_STR) || forceUpdate) {
        GOOnOffProperty* drawLaterProperty
            = static_cast<GOOnOffProperty*>(findProperty(GO_DRAW_LATER_PROPERTY_NAME_STR));
        if (!drawLaterProperty->asBool()) {
            repaint();
        }
        clearChanged(GO_DRAW_LATER_PROPERTY_NAME_STR);
    }
};
//=============================================================================
void
GOFigure::setOuterPosition(int x, int y, int w, int h)
{
}
//=============================================================================
void
GOFigure::getOuterPosition(int& x, int& y, int& w, int& h)
{
}
//=============================================================================
void
GOFigure::setInnerPosition(int x, int y, int w, int h)
{
}
//=============================================================================
void
GOFigure::getInnerPosition(int& x, int& y, int& w, int& h)
{
}
//=============================================================================
void
GOFigure::setPosition(int x, int y, int w, int h)
{
}
//=============================================================================
void
GOFigure::getPosition(int& x, int& y, int& w, int& h)
{
}
//=============================================================================
}
//=============================================================================
