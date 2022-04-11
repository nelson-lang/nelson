//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
#define BOOST_UUID_RANDOM_GENERATOR_COMPAT // BOOST 1.67
//=============================================================================
#include "CreateGUID.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
CreateGUID()
{
    boost::uuids::uuid uuid = boost::uuids::random_generator()();
    return boost::uuids::to_wstring(uuid);
}
//=============================================================================
wstringVector
CreateGUID(size_t nbGUID)
{
    wstringVector uuids;
    for (size_t k = 0; k < nbGUID; k++) {
        uuids.push_back(CreateGUID());
    }
    return uuids;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
