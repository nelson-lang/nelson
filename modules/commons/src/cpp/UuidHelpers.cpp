//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <random>
#include <uuid.hpp>
#include <wchar.h>
#include <mutex>
#include "UuidHelpers.hpp"
//=============================================================================
namespace Nelson::UuidHelpers {
//=============================================================================
std::mutex m_mutex;
//=============================================================================
static uuids::uuid
generateId()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    std::random_device rd;
    auto seed_data = std::array<int, std::mt19937::state_size> {};
    std::generate(std::begin(seed_data), std::end(seed_data), std::ref(rd));
    std::seed_seq seq(std::begin(seed_data), std::end(seed_data));
    std::mt19937 generator(seq);
    uuids::uuid_random_generator gen { generator };
    return gen();
}
//=============================================================================
void
generateUuid(std::wstring& wstr)
{
    uuids::uuid const id = generateId();
    wstr = uuids::to_string<wchar_t>(id);
}
//=============================================================================
void
generateUuid(std::string& str)
{
    uuids::uuid const id = generateId();
    str = uuids::to_string<char>(id);
}
//=============================================================================
}
//=============================================================================
