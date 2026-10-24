//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DataInterProcessToExchange.hpp"
#include "BinaryStream.hpp"
//=============================================================================
void
dataInterProcessToExchange::write(Nelson::BinaryWriter& writer) const
{
    writer.writePod<int32_t>(static_cast<int32_t>(commandType));
    switch (commandType) {
    case OPEN_FILES:
    case LOAD_FILES:
    case RUN_FILES:
    case OPEN_NFLOW_FILES: {
        writer.writeStringVector(filenames);
    } break;
    case EVAL: {
        writer.writePod<int32_t>(static_cast<int32_t>(pid));
        writer.writeString(content);
    } break;
    case POST_COMMAND: {
        writer.writeString(content);
        writer.writeString(scope);
    } break;
    case EVAL_ANSWER: {
        writer.writeString(content);
    } break;
    case PUT: {
        writer.writeString(serializedCompressedVariable);
        writer.writeU8(fullySerialized ? 1 : 0);
        writer.writeString(variableName);
        writer.writeString(scope);
    } break;
    case GET_ANSWER: {
        writer.writeString(serializedCompressedVariable);
        writer.writeU8(fullySerialized ? 1 : 0);
    } break;
    case GET:
    case IS_VAR: {
        writer.writePod<int32_t>(static_cast<int32_t>(pid));
        writer.writeString(variableName);
        writer.writeString(scope);
    } break;
    case IS_MINIMIZED: {
        writer.writePod<int32_t>(static_cast<int32_t>(pid));
    } break;
    case IS_MINIMIZED_ANSWER:
    case SET_MINIMIZE:
    case IS_VAR_ANSWER: {
        writer.writeU8(valueAnswer ? 1 : 0);
    } break;
    default: {
    } break;
    }
}
//=============================================================================
bool
dataInterProcessToExchange::read(Nelson::BinaryReader& reader)
{
    clear();
    int32_t command = 0;
    if (!reader.readPod(command)) {
        return false;
    }
    commandType = command;
    uint8_t flag = 0;
    int32_t pidValue = 0;
    switch (commandType) {
    case OPEN_FILES:
    case LOAD_FILES:
    case RUN_FILES:
    case OPEN_NFLOW_FILES: {
        return reader.readStringVector(filenames);
    }
    case EVAL: {
        if (!reader.readPod(pidValue)) {
            return false;
        }
        pid = pidValue;
        return reader.readString(content);
    }
    case POST_COMMAND: {
        return reader.readString(content) && reader.readString(scope);
    }
    case EVAL_ANSWER: {
        return reader.readString(content);
    }
    case PUT: {
        if (!reader.readString(serializedCompressedVariable) || !reader.readU8(flag)) {
            return false;
        }
        fullySerialized = (flag != 0);
        return reader.readString(variableName) && reader.readString(scope);
    }
    case GET_ANSWER: {
        if (!reader.readString(serializedCompressedVariable) || !reader.readU8(flag)) {
            return false;
        }
        fullySerialized = (flag != 0);
        return true;
    }
    case GET:
    case IS_VAR: {
        if (!reader.readPod(pidValue)) {
            return false;
        }
        pid = pidValue;
        return reader.readString(variableName) && reader.readString(scope);
    }
    case IS_MINIMIZED: {
        if (!reader.readPod(pidValue)) {
            return false;
        }
        pid = pidValue;
        return true;
    }
    case IS_MINIMIZED_ANSWER:
    case SET_MINIMIZE:
    case IS_VAR_ANSWER: {
        if (!reader.readU8(flag)) {
            return false;
        }
        valueAnswer = (flag != 0);
        return true;
    }
    default: {
    } break;
    }
    return true;
}
//=============================================================================
void
dataInterProcessToExchange::clear()
{
    valueAnswer = false;
    pid = 0;
    serializedCompressedVariable.clear();
    content.clear();
    variableName.clear();
    scope.clear();
}
//=============================================================================
bool
dataInterProcessToExchange::isFullySerialized()
{
    switch (commandType) {
    case OPEN_FILES: {
        return true;
    } break;
    case LOAD_FILES: {
        return true;
    } break;
    case RUN_FILES: {
        return true;
    } break;
    case OPEN_NFLOW_FILES: {
        return true;
    } break;
    case EVAL: {
        return true;
    } break;
    case GET: {
        return true;
    } break;
    case PUT:
    case GET_ANSWER:
    case IS_VAR_ANSWER: {
        return fullySerialized;
    } break;
    case IS_VAR: {
        return true;
    } break;
    case IS_MINIMIZED: {
        return true;
    } break;
    case IS_MINIMIZED_ANSWER: {
        return true;
    } break;
    case SET_MINIMIZE: {
        return true;
    } break;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
