//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <boost/container/vector.hpp>
#include <boost/unordered_map.hpp>
#include <vector>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
enum JSON_TO_NELSON_Type
{
    JSON_TO_NELSON_UNDEFINED, // unknow
                              // primitives
    JSON_TO_NELSON_LOGICAL, // logical
    JSON_TO_NELSON_DOUBLE, // double
    JSON_TO_NELSON_STRING, // char
                           // array
    JSON_TO_NELSON_ARRAY, // matrix of
    JSON_TO_NELSON_EMPTY_MATRIX, // []
                                 // object
    JSON_TO_NELSON_STRUCT, // struct
    JSON_TO_NELSON_CELL // cell
};
//=============================================================================
class JsonVariable
{
public:
    JSON_TO_NELSON_Type jsonVariableType;
    boost::container::vector<size_t> dims;

    std::string scalarString;
    double scalarDouble;
    logical scalarLogical;
    boost::unordered_map<std::string, JsonVariable> scalarMap;

    boost::container::vector<std::string> vectorString;
    boost::container::vector<double> vectorDouble;
    boost::container::vector<logical> vectorLogical;

    boost::container::vector<JsonVariable> vectorJsonVariable;
    boost::unordered_map<std::string, boost::container::vector<JsonVariable>> map;
    std::vector<std::string> fieldnames;

    bool reduced = false;

    JsonVariable();
    ~JsonVariable();
};
//=============================================================================
JsonVariable::JsonVariable()
{
    jsonVariableType = JSON_TO_NELSON_UNDEFINED;
    scalarDouble = std::nan("");
    scalarLogical = false;
};
//=============================================================================
JsonVariable::~JsonVariable(){};
//=============================================================================
}
//=============================================================================