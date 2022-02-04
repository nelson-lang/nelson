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
#include "GraphicObject.hpp"
#include "GOStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOVector2DProperty.hpp"
#include "GOVector3DProperty.hpp"
#include "GOVector4DProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOOnOffSwitchProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOColorProperty.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GraphicObject::GraphicObject() { refCount = 1; }
//=============================================================================
GraphicObject::~GraphicObject() = default;
//=============================================================================
void
GraphicObject::reference()
{
    refCount++;
}
//=============================================================================
void
GraphicObject::dereference()
{
    refCount--;
}
//=============================================================================
unsigned
GraphicObject::referenceCount()
{
    return refCount;
}
//=============================================================================
bool
GraphicObject::isType(const std::string& name) const
{
    return goType.compare(name) == 0;
}
//=============================================================================
std::string
GraphicObject::getType()
{
    return goType;
}
//=============================================================================
void
GraphicObject::setType(const std::string& objectType)
{
    goType = objectType;
}
//=============================================================================
void
GraphicObject::addProperty(const std::string& propertyName, GOProperty* goPropertyPtr)
{
    properties.emplace(propertyName, goPropertyPtr);
}
//=============================================================================
GOProperty*
GraphicObject::searchProperty(const std::string& name)
{
    if (properties.count(name) == 0) {
        return nullptr;
    }
    return properties[name];
}
//=============================================================================
stringVector
GraphicObject::getPropertiesName()
{
    stringVector names;
    for (auto p : properties) {
        names.push_back(p.first);
    }
    return names;
}
//=============================================================================
std::wstring
GraphicObject::displayProperties()
{
    refreshProperties();
    std::wstring content;
    for (auto p : properties) {
        auto propertyValue = p.second;
        auto propertyName = p.first;
        content = content + utf8_to_wstring(propertyValue->print(propertyName)) + L"\n";
    }
    return content;
}
//=============================================================================
void
GraphicObject::setPropertyAsStringValue(const std::string& propertyName, const std::string& value)
{
    auto* property = (GOStringProperty*)searchProperty(propertyName);
    if (property) {
        property->value(value);
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsScalarDoubleValue(const std::string& propertyName, double value)
{
    auto* property = (GOScalarDoubleProperty*)searchProperty(propertyName);
    if (property) {
        property->value(value);
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsScalarLogicalValue(const std::string& propertyName, bool value)
{
    auto* property = (GOScalarLogicalProperty*)searchProperty(propertyName);
    if (property) {
        property->value(static_cast<Nelson::logical>(value));
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsVector2DValue(const std::string& propertyName, double x1, double x2)
{
    auto* property = (GOVector2DProperty*)searchProperty(propertyName);
    if (property) {
        property->value(x1, x2);
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsVector3DValue(
    const std::string& propertyName, double x1, double x2, double x3)
{
    auto* property = (GOVector3DProperty*)searchProperty(propertyName);
    if (property) {
        property->value(x1, x2, x3);
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsVector4DValue(
    const std::string& propertyName, double x1, double x2, double x3, double x4)
{
    auto* property = (GOVector4DProperty*)searchProperty(propertyName);
    if (property) {
        property->value(x1, x2, x3, x4);
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsOnOffSwitchValue(
    const std::string& propertyName, const std::string& value)
{
    auto* property = (GOOnOffSwitchProperty*)searchProperty(propertyName);
    if (property) {
        property->value(value);
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsArrayOfValue(const std::string& propertyName, ArrayOf value)
{
    auto* property = (GOArrayOfProperty*)searchProperty(propertyName);
    if (property) {
        property->value(value);
    }
}
//=============================================================================
void
GraphicObject::setPropertyAsColorValue(
    const std::string& propertyName, double R, double G, double B)
{
    auto* property = (GOColorProperty*)searchProperty(propertyName);
    if (property) {
        property->value(R, G, B);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
