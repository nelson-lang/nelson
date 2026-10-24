%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--MPI MODE-->
%=============================================================================
mpiRequireRuntimeForTest();
%=============================================================================
if ~MPI_Initialized()
  MPI_Init();
end
num_ranks = MPI_Comm_size();
if num_ranks < 2
  if MPI_Initialized()
    MPI_Finalize();
  end
  skip_testsuite(true, 'MPI_Send requires at least two ranks.');
end
rank = MPI_Comm_rank();
tag = 79;
if rank == 0
  value = MPI_Recv(1, tag);
  asserts.isequal(value, 'mpi');
else
  MPI_Send('mpi', 0, tag);
end
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
