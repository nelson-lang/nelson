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
#include "TicToc.hpp"
#include <boost/chrono/chrono.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
Tic(Evaluator* eval)
{
    boost::chrono::nanoseconds ns = boost::chrono::high_resolution_clock::now().time_since_epoch();
    uint64 _now = uint64(static_cast<boost::uint64_t>(ns.count()));
    eval->TimerValue = _now;
    return true;
}
//=============================================================================
bool
Toc(Evaluator* eval, double& tValue)
{
    return Toc(eval->TimerValue, tValue);
}
//=============================================================================
bool
Toc(uint64 t, double& tValue)
{
    boost::chrono::nanoseconds ns = boost::chrono::high_resolution_clock::now().time_since_epoch();
    uint64 _now = uint64(static_cast<boost::uint64_t>(ns.count()));
    tValue = double(_now - t);
    tValue = tValue * 1e-9;
    return true;
}
//=============================================================================
}
//=============================================================================
