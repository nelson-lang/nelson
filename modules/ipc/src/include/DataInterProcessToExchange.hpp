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
#include <string>
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>
//=============================================================================
class dataInterProcessToExchange
{
    //=============================================================================
public:
    //=============================================================================
    dataInterProcessToExchange(int _pid, const std::string& _commandType,
        const std::string& compressedData, bool _fullySerialized)
        : pid(_pid)
        , commandType(_commandType)
        , serializedCompressedVariable(compressedData)
        , fullySerialized(_fullySerialized) {};
    //=============================================================================
    dataInterProcessToExchange(int _pid, const std::string& _commandType, bool value)
        : pid(_pid), commandType(_commandType), valueAnswer(value) {};

    //=============================================================================
    dataInterProcessToExchange(const std::string& _lineToEvaluate)
        : commandType("eval")
        , lineToEvaluate(_lineToEvaluate)
        , serializedCompressedVariable("")
        , variableName("")
        , scope("") {};
    //=============================================================================
    dataInterProcessToExchange(const std::string& _variableName, const std::string& _scope,
        const std::string& compressedData, bool _fullySerialized)
        : commandType("put")
        , serializedCompressedVariable(compressedData)
        , fullySerialized(_fullySerialized)
        , variableName(_variableName)
        , scope(_scope) {};
    //=============================================================================
    dataInterProcessToExchange(int _pid, const std::string& _commandType,
        const std::string& _variableName, const std::string& _scope)
        : pid(_pid), commandType(_commandType), variableName(_variableName), scope(_scope) {};
    //=============================================================================
    std::string serializedCompressedVariable;
    bool fullySerialized = false;
    int pid = 0;
    bool valueAnswer = false;
    std::string commandType;
    std::string lineToEvaluate;
    std::string variableName;
    std::string scope;
    //=============================================================================
    bool
    isFullySerialized();
    //=============================================================================
    void
    clear();
    //=============================================================================
private:
    //=============================================================================
    friend class boost::serialization::access;
    //=============================================================================
    template <class Archive>
    void
    serialize(Archive& ar, const unsigned int version)
    {
        ar& commandType;
        if (commandType == "eval") {
            ar& lineToEvaluate;
        }
        if (commandType == "isvar") {
            ar& pid;
            ar& variableName;
            ar& scope;
        }
        if (commandType == "put") {
            ar& serializedCompressedVariable;
            ar& fullySerialized;
            ar& variableName;
            ar& scope;
        }
        if (commandType == "isvar_answer") {
            ar& valueAnswer;
        }
        if (commandType == "get") {
            ar& pid;
            ar& variableName;
            ar& scope;
        }
        if (commandType == "get_answer") {
            ar& serializedCompressedVariable;
            ar& fullySerialized;
        }
    }
    //=============================================================================
};
//=============================================================================
