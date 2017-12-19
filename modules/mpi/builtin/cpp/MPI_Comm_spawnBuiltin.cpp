//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#ifdef _MSC_VER
#define _CRT_NONSTDC_NO_WARNINGS
#endif
#include <mpi.h>
#include "MPI_Comm_spawnBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_Comm_spawnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
	char **argv = nullptr;
	MPI_Info info;
	MPI_Info_create(&info);
	if ((argIn.size() == 0) || (argIn.size() > 5))
	{
		Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
	}
	if (nLhs > 1)
	{
		Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
	}
	ArrayOf param1 = argIn[0];
	std::string command = param1.getContentAsCString();
	bool argvAllocated = false;
	if (argIn.size() < 2)
	{
		argv = NULL;
	}
	else 
	{
		ArrayOf param2 = argIn[1];
		if (param2.isEmpty())
		{
			argv = nullptr;
		}
		else 
		{
			if (param2.isString()) 
			{
				argvAllocated = true;
				argv = (char**)malloc(sizeof(char*) * 2);
				argv[1] = nullptr;
				std::string paramstr = param2.getContentAsCString();
				argv[0] = strdup(paramstr.c_str());
			}
			else if (param2.getDataClass() == NLS_CELL_ARRAY) 
			{
				argvAllocated = true;
				ArrayOf *dp = (ArrayOf*)param2.getDataPointer();
				int len = (int)param2.getLength();
				argv = (char**)malloc(sizeof(char*)*(len + 1));
				argv[len] = 0;
				for (int m = 0; m < len; m++) 
				{
					ArrayOf q = dp[m];
					std::string paramstr = q.getContentAsCString();
					argv[m] = strdup(paramstr.c_str());
				}
			}
			else
			{
				Error(eval, _W("#2 input argument: string or cell of strings expected."));
			}
		}
	}
	int maxprocs = 1;
	if (argIn.size() >= 3)
	{
		ArrayOf param3 = argIn[2];
		maxprocs = param3.getContentAsInteger32Scalar();
	}
	int root = 0;
	if (argIn.size() >= 4)
	{
		ArrayOf param4 = argIn[3];
		root = param4.getContentAsInteger32Scalar();
	}
	MPI_Comm comm = MPI_COMM_SELF;
	if (argIn.size() == 5)
	{
		comm = HandleToMpiComm(argIn[4]);
	}
	int *errcodes = (int*) ArrayOf::allocateArrayOf(NLS_INT32, maxprocs);
	MPI_Comm intercomm = MPI_COMM_NULL;
	int res = MPI_Comm_spawn(command.c_str(), argv, maxprocs, info, root, comm, &intercomm, errcodes);
	retval.push_back(MpiCommToHandle(intercomm));
	Dimensions dim;
	dim[0] = maxprocs;
	dim[1] = 1;
	retval.push_back(ArrayOf::ArrayOf(NLS_INT32, dim, errcodes));
	if (argvAllocated)
	{
		free(argv);
		argv = nullptr;
	}
    return retval;
}
//=============================================================================
