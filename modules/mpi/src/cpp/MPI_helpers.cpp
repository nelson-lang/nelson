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
#include <mpi.h>
#include <boost/container/vector.hpp>
#include "MPI_helpers.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
	static MPI_Errhandler errhdl;
	//=============================================================================
	void MPIErrorHandler(MPI_Comm *comm, int *errorcode, ...) 
	{
		char buffer[MPI_MAX_ERROR_STRING];
		int resultlen = 0;
		MPI_Error_string(*errorcode, buffer, &resultlen);
		buffer[resultlen] = 0;
		throw Exception(buffer);
	}
	//=============================================================================
	int initializeMPI()
	{
		int flag;
		MPI_Initialized(&flag);
		if (flag) 
		{
			return flag;
		}
		MPI_Init(NULL, NULL);
		MPI_Comm_create_errhandler(MPIErrorHandler, &errhdl);
		MPI_Comm_set_errhandler(MPI_COMM_WORLD, errhdl);
		MPI_Initialized(&flag);
		return flag;
	}
	//=============================================================================
}
//=============================================================================
