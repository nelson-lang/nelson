//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>
#include "nlsConfig.h"
#include "ComputionalThreads.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static unsigned int nbOfThreadsToUse = 0;
//=============================================================================
unsigned int
getMaxNumCompThreads()
{
    return nbOfThreadsToUse;
}
//=============================================================================
unsigned int
setMaxNumCompThreads(unsigned int _nbOfCores)
{
    unsigned int previousValue = nbOfThreadsToUse;
    nbOfThreadsToUse = _nbOfCores;
#if defined(_NLS_WITH_OPENMP)
    omp_set_num_threads(_nbOfCores);
#endif
    Eigen::setNbThreads(_nbOfCores);
    return previousValue;
}
//=============================================================================
unsigned int
setDefaultMaxNumCompThreads()
{
    std::wstring omp_env = GetVariableEnvironment(L"OMP_NUM_THREADS", L"0");
    if (omp_env == L"0") {
        nbOfThreadsToUse = boost::thread::hardware_concurrency();
    } else {
        try {
            nbOfThreadsToUse = boost::lexical_cast<unsigned int>(omp_env.c_str());
        } catch (const boost::bad_lexical_cast&) {
            nbOfThreadsToUse = 0;
        }
        if (nbOfThreadsToUse == 0) {
            nbOfThreadsToUse = boost::thread::hardware_concurrency();
        }
    }
    setMaxNumCompThreads(nbOfThreadsToUse);
    return nbOfThreadsToUse;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
