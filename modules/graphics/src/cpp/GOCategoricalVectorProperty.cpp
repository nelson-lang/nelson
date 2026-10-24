//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "GOCategoricalVectorProperty.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOCategoricalVectorProperty::GOCategoricalVectorProperty(bool finiteOnly, bool emptyAsMatrix)
    : GOVectorProperty(finiteOnly, emptyAsMatrix)
{
}
//=============================================================================
static bool
isCategoricalValue(const ArrayOf& value)
{
    return value.isClassType() && ClassName(value) == "categorical";
}
//=============================================================================
static ArrayOf
categoricalCodes(const ArrayOf& value)
{
    ArrayOf codes = value.field("codes");
    codes.promoteType(NLS_DOUBLE);
    return codes;
}
//=============================================================================
ArrayOf
GOCategoricalVectorProperty::get()
{
    if (_hasCategoricalValue) {
        return _categoricalValue;
    }
    return GOVectorProperty::get();
}
//=============================================================================
void
GOCategoricalVectorProperty::set(ArrayOf value)
{
    if (isCategoricalValue(value)) {
        GOVectorProperty::set(categoricalCodes(value));
        _categoricalValue = value;
        _hasCategoricalValue = true;
        return;
    }
    clearCategoricalValue();
    GOVectorProperty::set(value);
}
//=============================================================================
void
GOCategoricalVectorProperty::data(const std::vector<double>& m)
{
    clearCategoricalValue();
    GOVectorProperty::data(m);
}
//=============================================================================
void
GOCategoricalVectorProperty::categoricalValue(const ArrayOf& value)
{
    _categoricalValue = value;
    _hasCategoricalValue = true;
}
//=============================================================================
void
GOCategoricalVectorProperty::clearCategoricalValue()
{
    _categoricalValue = ArrayOf::empty();
    _hasCategoricalValue = false;
}
//=============================================================================
ArrayOf
GOCategoricalTwoVectorProperty::get()
{
    if (_hasCategoricalValue) {
        return _categoricalValue;
    }
    return GOTwoVectorProperty::get();
}
//=============================================================================
void
GOCategoricalTwoVectorProperty::set(ArrayOf value)
{
    if (isCategoricalValue(value)) {
        GOTwoVectorProperty::set(categoricalCodes(value));
        _categoricalValue = value;
        _hasCategoricalValue = true;
        return;
    }
    ArrayOf asDouble(value);
    asDouble.promoteType(NLS_DOUBLE);
    const double* dp = static_cast<const double*>(asDouble.dataPointer());
    std::vector<double> values(2, 0);
    indexType count = std::min(static_cast<indexType>(2), asDouble.elementCount());
    for (indexType k = 0; k < count; k++) {
        values[k] = dp[k];
    }
    clearCategoricalValue();
    GOVectorProperty::data(values);
    GOGenericProperty::set(value);
}
//=============================================================================
void
GOCategoricalTwoVectorProperty::data(const std::vector<double>& m)
{
    clearCategoricalValue();
    GOVectorProperty::data(m);
}
//=============================================================================
void
GOCategoricalTwoVectorProperty::categoricalValue(const ArrayOf& value)
{
    _categoricalValue = value;
    _hasCategoricalValue = true;
}
//=============================================================================
void
GOCategoricalTwoVectorProperty::clearCategoricalValue()
{
    _categoricalValue = ArrayOf::empty();
    _hasCategoricalValue = false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
