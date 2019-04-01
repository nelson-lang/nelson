//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include <boost/chrono/chrono.hpp>
#include "Profiler.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Profiler* Profiler::m_pInstance = nullptr;
//=============================================================================
Profiler::Profiler() { profiling.clear(); }
//=============================================================================
Profiler*
Profiler::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new Profiler();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
Profiler::on()
{
    clear();
    resume();
}
//=============================================================================
void
Profiler::off()
{
    profileOn = false;
}
//=============================================================================
void
Profiler::resume()
{
    profileOn = true;
}
//=============================================================================
void
Profiler::clear()
{
    profiling.clear();
}
//=============================================================================
void
Profiler::tic(const std::string& functionName, const std::wstring& filename)
{
    if (!profileOn) {
        return;
    }
    if (functionName == "profile") {
        return;
    }
}
//=============================================================================
void
Profiler::toc(const std::string& functionName, const std::wstring& filename)
{
    if (!profileOn) {
        return;
    }
    if (functionName == "profile") {
        return;
    }
}
//=============================================================================
std::unordered_map<std::string, std::vector<uint64>>
Profiler::info()
{
    return profiling;
}
//=============================================================================
uint64
Profiler::now()
{
  boost::chrono::nanoseconds ns
    = boost::chrono::high_resolution_clock::now().time_since_epoch();
  return uint64(static_cast<boost::uint64_t>(ns.count()));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
