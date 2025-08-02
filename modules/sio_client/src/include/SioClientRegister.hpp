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
#include "nlsSio_client_exports.h"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSIO_CLIENT_IMPEXP void
sioregister(const std::string& name, const std::string& function_name);
//=============================================================================
NLSSIO_CLIENT_IMPEXP stringVector
sioregisterList();
//=============================================================================
NLSSIO_CLIENT_IMPEXP bool
issioreserved(const std::string& name);
//=============================================================================
NLSSIO_CLIENT_IMPEXP bool
issioregistered(const std::string& name);
//=============================================================================
NLSSIO_CLIENT_IMPEXP void
siounregister(const std::string& name);
//=============================================================================
} // namespace Nelson;
//=============================================================================
