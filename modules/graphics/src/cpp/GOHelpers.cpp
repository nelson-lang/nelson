//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOList.hpp"
#include "GOFiguresManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GOList<GraphicsObject*> objectset;
//=============================================================================
bool
isDeletedGraphicsObject(int64 handle)
{
    if (handle == HANDLE_ROOT_OBJECT) {
        return false;
    }
    if (handle >= HANDLE_OFFSET_OBJECT) {
        return (findGraphicsObject(handle) == nullptr);
    }
    return (getFigure(handle) == nullptr);
}
//=============================================================================

void
freeGraphicsObject(int64 handle)
{
    objectset.deleteGO(handle - HANDLE_OFFSET_OBJECT);
}
//=============================================================================
void
checkIdValidity(int64 id)
{
    bool isValidId = (id >= 0 && id < (int64)std::numeric_limits<int>::max());
    if (!isValidId) {
        Error(_("Invalid figure id."));
    }
}
//=============================================================================
void
Tokenize(const std::wstring& str, std::vector<std::wstring>& tokens, const std::wstring& delimiters)
{
    std::wstring::size_type lastPos = str.find_first_not_of(delimiters, 0);
    std::wstring::size_type pos = str.find_first_of(delimiters, lastPos);

    while (std::wstring::npos != pos || std::wstring::npos != lastPos) {
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        lastPos = str.find_first_not_of(delimiters, pos);
        pos = str.find_first_of(delimiters, lastPos);
    }
}
//=============================================================================
void
validateGO(int64 handle)
{
    if (handle == 0)
        return;
    if (handle >= HANDLE_OFFSET_OBJECT)
        findGraphicsObject(handle);
    else
        findGOFigure(handle);
}
//=============================================================================
GraphicsObject*
findGraphicsObject(int64 handle)
{
    return (objectset.findGO(handle - HANDLE_OFFSET_OBJECT));
}
//=============================================================================
GOFigure*
findGOFigure(int64 handle)
{
    int64 id = handle;
    checkIdValidity(id);
    GOWindow* window = getFigure(id);
    if (window) {
        return window->getGOFigure();
    }
    selectFigure(id);
    window = getFigure(id);
    if (window) {
        return window->getGOFigure();
    }
    return nullptr;
}
//=============================================================================
GOWindow*
findGOWindows(int64 handle)
{
    int64 id = handle;
    checkIdValidity(id);
    return getFigure(id);
}
//=============================================================================
int64
assignGraphicsObject(GraphicsObject* hp)
{
    return (objectset.assignGO(hp) + HANDLE_OFFSET_OBJECT);
}
//=============================================================================
}
//=============================================================================
