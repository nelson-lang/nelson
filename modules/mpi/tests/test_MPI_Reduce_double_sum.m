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
% mpiexec -n 2 NelSon-cli.exe -e run('test_MPI_Reduce_double_sum.m');exit
if ~MPI_Initialized()
  MPI_Init();
end
my_rank = MPI_Comm_rank ();
num_ranks = MPI_Comm_size();

A = [1 + my_rank:3 + my_rank];
B = MPI_Reduce(A, 'MPI_SUM', 0);
if (my_rank == 0)
  assert_isequal(B, [3 5 7]);
end
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
