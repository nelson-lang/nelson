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
#include <utility>
//=============================================================================
typedef enum
{
    OPEN_FILES,
    LOAD_FILES,
    RUN_FILES,
    EVAL,
    EVAL_ANSWER,
    POST_COMMAND,
    PUT,
    GET,
    GET_ANSWER,
    IS_VAR,
    IS_VAR_ANSWER,
    IS_MINIMIZED,
    IS_MINIMIZED_ANSWER,
    SET_MINIMIZE,
    UNKNOWN
} NELSON_INTERPROCESS_COMMAND;
//=============================================================================
class dataInterProcessToExchange
{
    //=============================================================================
public:
    //=============================================================================
    dataInterProcessToExchange(int _pid, NELSON_INTERPROCESS_COMMAND _commandType,
        std::string compressedData, bool _fullySerialized)
        : pid(_pid)
        , commandType(_commandType)
        , serializedCompressedVariable(std::move(compressedData))
        , fullySerialized(_fullySerialized){};
    //=============================================================================
    dataInterProcessToExchange(int _pid, NELSON_INTERPROCESS_COMMAND _commandType)
        : pid(_pid), commandType(_commandType){};
    //=============================================================================
    dataInterProcessToExchange(
        int _pid, NELSON_INTERPROCESS_COMMAND _commandType, const std::string& content)
        : pid(_pid), commandType(_commandType), lineToEvaluate(std::move(content)){};
    //=============================================================================
    dataInterProcessToExchange(int _pid, NELSON_INTERPROCESS_COMMAND _commandType, bool value)
        : pid(_pid), commandType(_commandType), valueAnswer(value){};
    //=============================================================================
    dataInterProcessToExchange(
        NELSON_INTERPROCESS_COMMAND _commandType, const std::vector<std::string>& _filenames)
        : commandType(_commandType), filenames(_filenames){};
    //=============================================================================
    dataInterProcessToExchange(std::string _lineToEvaluate)
        : commandType(NELSON_INTERPROCESS_COMMAND::POST_COMMAND)
        , lineToEvaluate(std::move(_lineToEvaluate))
        , serializedCompressedVariable("")
        , variableName("")
        , scope(""){};
    //=============================================================================
    dataInterProcessToExchange(int _pid, std::string _lineToEvaluate)
        : pid(_pid)
        , commandType(NELSON_INTERPROCESS_COMMAND::EVAL)
        , lineToEvaluate(std::move(_lineToEvaluate))
        , serializedCompressedVariable("")
        , variableName("")
        , scope(""){};
    //=============================================================================
    dataInterProcessToExchange(std::string _variableName, std::string _scope,
        std::string compressedData, bool _fullySerialized)
        : commandType(NELSON_INTERPROCESS_COMMAND::PUT)
        , serializedCompressedVariable(std::move(compressedData))
        , fullySerialized(_fullySerialized)
        , variableName(std::move(_variableName))
        , scope(std::move(_scope)){};
    //=============================================================================
    dataInterProcessToExchange(int _pid, NELSON_INTERPROCESS_COMMAND _commandType,
        std::string _variableName, std::string _scope)
        : pid(_pid)
        , commandType(_commandType)
        , variableName(std::move(_variableName))
        , scope(std::move(_scope)){};
    //=============================================================================
    std::string serializedCompressedVariable;
    bool fullySerialized = false;
    int pid = 0;
    bool valueAnswer = false;
    int commandType;
    std::string lineToEvaluate;
    std::string variableName;
    std::string scope;
    std::vector<std::string> filenames;
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
        switch (commandType) {
        case OPEN_FILES: {
            ar& filenames;
        } break;
        case LOAD_FILES: {
            ar& filenames;
        } break;
        case RUN_FILES: {
            ar& filenames;
        } break;
        case POST_COMMAND: {
            ar& lineToEvaluate;
        } break;
        case EVAL: {
            ar& pid;
            ar& lineToEvaluate;
        } break;
        case EVAL_ANSWER: {
            ar& lineToEvaluate;
        } break;
        case PUT: {
            ar& serializedCompressedVariable;
            ar& fullySerialized;
            ar& variableName;
            ar& scope;
        } break;
        case GET: {
            ar& pid;
            ar& variableName;
            ar& scope;
        } break;
        case GET_ANSWER: {
            ar& serializedCompressedVariable;
            ar& fullySerialized;
        } break;
        case IS_VAR: {
            ar& pid;
            ar& variableName;
            ar& scope;
        } break;
        case IS_MINIMIZED: {
            ar& pid;
        } break;
        case IS_MINIMIZED_ANSWER:
        case SET_MINIMIZE:
        case IS_VAR_ANSWER: {
            ar& valueAnswer;
        } break;
        default: { } break; }
    }
    //=============================================================================
};
//=============================================================================
