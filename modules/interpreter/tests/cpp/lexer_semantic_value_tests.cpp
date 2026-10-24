//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LexerContext.hpp"
#include <cassert>
#include <cstdint>
//=============================================================================
int
main()
{
    Nelson::ParseRHS token {};
    token.isToken = true;
    token.v.i = 0;
    Nelson::setTokenContextIfMissing(token, 0x0009003B);
    assert(token.v.i == 0x0009003B);

    if (sizeof(void*) > sizeof(int)) {
        Nelson::ParseRHS node {};
        node.isToken = false;
        node.v.p = reinterpret_cast<Nelson::AbstractSyntaxTreePtr>(
            static_cast<std::uintptr_t>(0x0000600000000000ULL));
        const Nelson::AbstractSyntaxTreePtr original = node.v.p;
        Nelson::setTokenContextIfMissing(node, 0x0009003B);
        assert(node.v.p == original);
    }
    return 0;
}
//=============================================================================
