%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--MPI MODE-->
%=============================================================================
% mpiexec -n 2 NelSon-cli.exe -e run('test_MPI_Reduce_double_prod.m');exit
if ~MPI_Initialized()
  MPI_Init();
end
comm = MPI_Comm_object();
my_rank = MPI_Comm_rank ();
num_ranks = MPI_Comm_size();

A = [1 + my_rank:3 + my_rank];
B = MPI_Allreduce(A, 'MPI_PROD', comm);
if (my_rank == 0)
  assert_isequal(B, [2, 6, 12]);
end
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
