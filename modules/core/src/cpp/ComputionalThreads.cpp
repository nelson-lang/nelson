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
#if defined(_OPENMP)
#include <omp.h>
#endif
#include "ComputionalThreads.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include <Eigen/Dense>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static unsigned int nbOfCoresToUse = 0;
//=============================================================================
unsigned int
getMaxNumCompThreads()
{
    return nbOfCoresToUse;
}
//=============================================================================
unsigned int
setMaxNumCompThreads(unsigned int _nbOfCores)
{
    unsigned int previousValue = nbOfCoresToUse;
    nbOfCoresToUse = _nbOfCores;
#if defined(_OPENMP)
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
        nbOfCoresToUse = boost::thread::physical_concurrency();
    } else {
        try {
            nbOfCoresToUse = boost::lexical_cast<unsigned int>(omp_env.c_str());
        } catch (const boost::bad_lexical_cast&) {
            nbOfCoresToUse = 0;
        }
        if (nbOfCoresToUse == 0) {
            nbOfCoresToUse = boost::thread::physical_concurrency();
        }
    }
    setMaxNumCompThreads(nbOfCoresToUse);
    return nbOfCoresToUse;
}
//=============================================================================
}
//=============================================================================
