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
#include <algorithm>
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOList.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "IsCellOfStrings.hpp"
#include "StringHelpers.hpp"
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
        return (findGraphicsObject(handle, false) == nullptr);
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
ArrayOf
uniformizeStringVector(const ArrayOf& arg, wstringVector& asWideStringVector)
{
    ArrayOf res;
    if (arg.isRowVectorCharacterArray()) {
        std::wstring str = arg.getContentAsWideString();
        if (StringHelpers::contains(str, L"|")) {
            Tokenize(str, asWideStringVector, L"|");
        } else {
            asWideStringVector.push_back(str);
        }
        Dimensions dims(asWideStringVector.size(), 1);
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, asWideStringVector.size());
        res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        for (indexType k = 0; k < dims.getElementCount(); k++) {
            elements[k] = ArrayOf::characterArrayConstructor(asWideStringVector[k]);
        }
    } else if (arg.isStringArray()) {
        asWideStringVector = arg.getContentAsWideStringVector();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, asWideStringVector.size());
        Dimensions dims(asWideStringVector.size(), 1);
        res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        for (indexType k = 0; k < dims.getElementCount(); k++) {
            elements[k] = ArrayOf::characterArrayConstructor(asWideStringVector[k]);
        }
    } else if (IsCellOfString(arg)) {
        asWideStringVector = arg.getContentAsWideStringVector();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, asWideStringVector.size());
        Dimensions dims(asWideStringVector.size(), 1);
        res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        for (indexType k = 0; k < dims.getElementCount(); k++) {
            elements[k] = ArrayOf::characterArrayConstructor(asWideStringVector[k]);
        }
    } else {
        Error(_W("The value must be a '|' delimited character vector, string array, or cell array "
                 "of character vectors."));
    }
    return res;
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
bool
deleteGraphicsObject(int64 handle, bool repaintParentFigure)
{
    GraphicsObject* gp = findGraphicsObject(handle, false);
    if (!gp) {
        return false;
    }
    GOFigure* parentFigure = nullptr;
    if (!gp->isType(L"figure") && !gp->isType(L"root")) {
        parentFigure = gp->getParentFigure();
    }
    GOGObjectsProperty* hp = (GOGObjectsProperty*)gp->findProperty(GO_PARENT_PROPERTY_NAME_STR);
    std::vector<int64> parents(hp->data());
    if (!parents.empty()) {
        for (auto p : parents) {
            GraphicsObject* gparent = findGraphicsObject(p, false);
            if (gparent) {
                GOGObjectsProperty* gchildren = (GOGObjectsProperty*)gparent->findProperty(
                    GO_CHILDREN_PROPERTY_NAME_STR, false);
                if (gchildren) {
                    std::vector<int64> children(gchildren->data());
                    if (!children.empty()) {
                        std::vector<int64> filtered;
                        filtered.reserve(children.size());
                        for (auto c : children) {
                            if (c != handle) {
                                filtered.push_back(c);
                            }
                        }
                        gchildren->data(filtered);
                    }
                }
            }
        }
    }
    freeGraphicsObject(handle);
    if (parentFigure && repaintParentFigure) {
        parentFigure->repaint();
    }
    return true;
}
//=============================================================================
GraphicsObject*
findGraphicsObject(int64 handle, bool throwError)
{
    return objectset.findGO(handle - HANDLE_OFFSET_OBJECT, throwError);
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
