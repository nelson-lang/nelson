//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOTransform.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "RenderInterface.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOTransform::~GOTransform() = default;
//=============================================================================
GOTransform::GOTransform()
{
    // The GOGroup base constructor built the group properties while the group vtable
    // was active (Type == "hggroup"). Add the Matrix property, set the type to "hgtransform"
    // and re-sort so the property set stays ordered.
    registerProperty(new GOArrayOfProperty, GO_MATRIX_PROPERTY_NAME_STR);
    Dimensions dims(4, 4);
    double* m = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
    for (indexType i = 0; i < dims.elementCount(); ++i) {
        m[i] = 0.0;
    }
    m[0] = 1.0;
    m[5] = 1.0;
    m[10] = 1.0;
    m[15] = 1.0;
    findProperty(GO_MATRIX_PROPERTY_NAME_STR)->set(ArrayOf(NLS_DOUBLE, dims, m));
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HGTRANSFORM_STR);
    sortProperties();
}
//=============================================================================
std::wstring
GOTransform::getType()
{
    return GO_PROPERTY_VALUE_HGTRANSFORM_STR;
}
//=============================================================================
std::string
GOTransform::getClassName()
{
    return GO_CLASS_NAME_TRANSFORM_STR;
}
//=============================================================================
void
GOTransform::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOArrayOfProperty* matrixProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_MATRIX_PROPERTY_NAME_STR));
    ArrayOf matrix(matrixProperty->data());
    bool pushed = false;
    if (matrix.isNumeric() && matrix.dimensions().elementCount() == 16) {
        matrix.promoteType(NLS_DOUBLE);
        gc.pushModelMatrix(static_cast<const double*>(matrix.dataPointer()));
        pushed = true;
    }
    GOGroup::paintMe(gc);
    if (pushed) {
        gc.popModelMatrix();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
