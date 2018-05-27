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
}
//=============================================================================
