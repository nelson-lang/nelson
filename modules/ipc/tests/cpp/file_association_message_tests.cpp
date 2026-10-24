//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BinaryStream.hpp"
#include "DataInterProcessToExchange.hpp"
#include <iostream>
#include <string>
#include <vector>
//=============================================================================
static bool
check(bool condition, const char* message)
{
    if (!condition) {
        std::cerr << message << '\n';
    }
    return condition;
}
//=============================================================================
int
main()
{
    static_assert(OPEN_FILES == 0);
    static_assert(SET_MINIMIZE == 13);
    static_assert(OPEN_NFLOW_FILES == 14);

    const std::vector<std::string> filenames { "model.nflow", "second model.nflow" };
    dataInterProcessToExchange outgoing(OPEN_NFLOW_FILES, filenames);
    if (!check(outgoing.isFullySerialized(), "NFlow file messages must be serializable")) {
        return 1;
    }

    std::string bytes;
    Nelson::BinaryWriter writer(bytes);
    outgoing.write(writer);

    dataInterProcessToExchange incoming(0, UNKNOWN);
    Nelson::BinaryReader reader(bytes);
    if (!check(incoming.read(reader), "NFlow file messages must deserialize")) {
        return 1;
    }
    if (!check(reader.atEnd(), "NFlow file messages must consume their complete payload")) {
        return 1;
    }
    if (!check(incoming.commandType == OPEN_NFLOW_FILES,
            "NFlow file messages must preserve their command type")) {
        return 1;
    }
    if (!check(incoming.filenames == filenames, "NFlow file messages must preserve file paths")) {
        return 1;
    }
    return 0;
}
//=============================================================================
