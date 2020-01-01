//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include "ArrayOf.hpp"
#include "GOProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOVector4DProperty : public GOProperty
{
protected:
    std::vector<double> m_data;

public:
    //=============================================================================
    GOVector4DProperty() { m_data.reserve(4); }
    //=============================================================================
    ~GOVector4DProperty() override { m_data.clear(); }
    //=============================================================================
    ArrayOf
    get() override;
    //=============================================================================
    void set(ArrayOf /*unused*/) override;
    //=============================================================================
    std::vector<double>
    data()
    {
        return m_data;
    }
    //=============================================================================
    void
    value(double x1, double x2, double x3, double x4)
    {
        if (!m_data.empty()) {
            m_data.clear();
            m_data.reserve(4);
        }
        m_data.push_back(x1);
        m_data.push_back(x2);
        m_data.push_back(x3);
        m_data.push_back(x4);
    }
    //=============================================================================
    std::string
    print(const std::string& propertyName) override
    {
        return "\t" + propertyName + ":\t [ " + std::to_string(m_data[0]) + " "
            + std::to_string(m_data[1]) + " " + std::to_string(m_data[2]) + " "
            + std::to_string(m_data[3]) + " ]";
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
