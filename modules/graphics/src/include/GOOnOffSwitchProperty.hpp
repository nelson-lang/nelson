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
#include "ArrayOf.hpp"
#include "GOProperty.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOOnOffSwitchProperty : public GOProperty
{
protected:
    logical m_data;

public:
    //=============================================================================
    GOOnOffSwitchProperty() = default;
    //=============================================================================
    ~GOOnOffSwitchProperty() override = default;
    //=============================================================================
    ArrayOf
    get() override;
    //=============================================================================
    void set(ArrayOf /*value*/) override;
    //=============================================================================
    logical
    asLogical()
    {
        return m_data;
    }
    //=============================================================================
    std::string
    data()
    {
        return (m_data == 0) ? "off" : "on";
    }
    //=============================================================================
    std::string
    print(const std::string& propertyName) override
    {
        std::string res = "\t" + propertyName + ":\t";
        std::string v = (m_data == 0) ? "off" : "on";
        return res + v;
    }
    //=============================================================================
    void
    value(const std::string& m)
    {
        if (m == "on") {
            m_data = true;
        } else if (m == "off") {
            m_data = false;
        } else {
            Error(_W("'off' or 'on' expected."));
        }
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
