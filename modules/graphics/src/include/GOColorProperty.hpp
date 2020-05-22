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
class GOColorProperty : public GOProperty
{
protected:
    double R;
    double G;
    double B;

public:
    //=============================================================================
    GOColorProperty() = default;
    //=============================================================================
    ~GOColorProperty() override = default;
    //=============================================================================
    ArrayOf
    get() override;
    //=============================================================================
    void set(ArrayOf /*value*/) override;
    //=============================================================================
    std::vector<double>
    asVector()
    {
        std::vector<double> rgb;
        rgb.push_back(R);
        rgb.push_back(G);
        rgb.push_back(B);
        return rgb;
    }
    //=============================================================================
    std::vector<double>
    data()
    {
        return asVector();
    }
    //=============================================================================
    std::string
    print(const std::string& propertyName) override;
    //=============================================================================
    void
    value(double r, double g, double b)
    {
        R = r;
        G = g;
        B = b;
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
