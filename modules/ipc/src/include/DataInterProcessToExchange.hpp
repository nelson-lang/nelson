//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include <utility>
//=============================================================================
namespace Nelson {
class BinaryWriter;
class BinaryReader;
//=============================================================================
} // namespace Nelson
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
    OPEN_NFLOW_FILES,
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
        , fullySerialized(_fullySerialized) { };
    //=============================================================================
    dataInterProcessToExchange(int _pid, NELSON_INTERPROCESS_COMMAND _commandType)
        : pid(_pid), commandType(_commandType) { };
    //=============================================================================
    dataInterProcessToExchange(
        int _pid, NELSON_INTERPROCESS_COMMAND _commandType, const std::string& content)
        : pid(_pid), commandType(_commandType), content(std::move(content)) { };
    //=============================================================================
    dataInterProcessToExchange(int _pid, NELSON_INTERPROCESS_COMMAND _commandType, bool value)
        : pid(_pid), commandType(_commandType), valueAnswer(value) { };
    //=============================================================================
    dataInterProcessToExchange(
        NELSON_INTERPROCESS_COMMAND _commandType, const std::vector<std::string>& _filenames)
        : commandType(_commandType), filenames(_filenames) { };
    //=============================================================================
    dataInterProcessToExchange(const std::string& _variableName, const std::string& _scope,
        const std::string& compressedData, bool _fullySerialized)
        : commandType(NELSON_INTERPROCESS_COMMAND::PUT)
        , serializedCompressedVariable(std::move(compressedData))
        , fullySerialized(_fullySerialized)
        , variableName(std::move(_variableName))
        , scope(std::move(_scope)) { };
    //=============================================================================
    dataInterProcessToExchange(int _pid, NELSON_INTERPROCESS_COMMAND _commandType,
        const std::string& _variableName, const std::string& _scope)
        : pid(_pid), commandType(_commandType), scope(std::move(_scope))
    {
        if (_commandType == NELSON_INTERPROCESS_COMMAND::POST_COMMAND) {
            content = _variableName;
        } else {
            variableName = _variableName;
        }
    };
    //=============================================================================
    std::string serializedCompressedVariable;
    bool fullySerialized = false;
    int pid = 0;
    bool valueAnswer = false;
    int commandType;
    std::string content;
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
    void
    write(Nelson::BinaryWriter& writer) const;
    //=============================================================================
    bool
    read(Nelson::BinaryReader& reader);
    //=============================================================================
};
//=============================================================================
