//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include <limits>
#include "GOVectorProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOVectorProperty::GOVectorProperty(bool finiteOnly) { _finiteOnly = finiteOnly; }
//=============================================================================
ArrayOf
GOVectorProperty::get()
{
    ArrayOf ret(ArrayOf::doubleVectorConstructor(_data.size()));
    double* dp = (double*)ret.getReadWriteDataPointer();
    OMP_PARALLEL_FOR_LOOP(_data.size())
    for (ompIndexType i = 0; i < (ompIndexType)_data.size(); i++) {
        dp[i] = _data[i];
    }
    return ret;
}
//=============================================================================
void
GOVectorProperty::set(ArrayOf num)
{
    if (_finiteOnly) {
        ArrayOf asDouble(num);
        asDouble.promoteType(NLS_DOUBLE);
        std::vector<double> values;
        values.reserve(asDouble.getElementCount());
        const double* dp = (const double*)asDouble.getDataPointer();
        for (indexType i = 0; i < asDouble.getElementCount(); i++) {
            if (!std::isfinite(dp[i])) {
                Error(_W("Finite value expected."));
            }
            values.push_back(dp[i]);
        }
        _data = values;
        GOGenericProperty::set(num);
    } else {
        GOGenericProperty::set(num);
        num.promoteType(NLS_DOUBLE);
        const double* dp = (const double*)num.getDataPointer();
        _data.clear();
        _data.reserve(num.getElementCount());
        for (indexType i = 0; i < num.getElementCount(); i++) {
            _data.push_back(dp[i]);
        }
    }
}
//=============================================================================
double&
GOVectorProperty::at(int ndx)
{
    if (_data.size() <= ndx) {
        _data.resize(ndx + 1);
        _data[ndx] = 0.;
    }
    return _data[ndx];
}
//=============================================================================
double&
GOVectorProperty::operator[](int ndx)
{
    return at(ndx);
}
//=============================================================================
std::vector<double>
GOVectorProperty::data()
{
    return _data;
}
//=============================================================================
void
GOVectorProperty::data(const std::vector<double>& m)
{
    if (_finiteOnly) {
        for (auto v : m) {
            if (!std::isfinite(v)) {
                Error(_W("Finite value expected."));
            }
        }
    }
    _data = m;
}
//=============================================================================
std::wstring
GOVectorProperty::toWideString()
{
    std::wstring msg = L"[";
    for (size_t k = 0; k < _data.size(); k++) {
        if (std::fabs((double)(int)(_data[k]) - _data[k])
            < std::numeric_limits<double>::epsilon()) {
            msg = msg + std::to_wstring((int)_data[k]);
        } else {
            msg = msg + std::to_wstring(_data[k]);
        }
        if (k < _data.size() - 1) {
            msg = msg + L" ";
        }
    }
    return msg + L"]";
}
//=============================================================================
}
//=============================================================================
