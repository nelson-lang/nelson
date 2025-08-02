%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% mpiexec([modulepath('mpi'), '/examples/MPI_parallel_sum.m'], 40)
%=============================================================================
if ~MPI_Initialized()
  MPI_Init();
end
%=============================================================================
N = 1000000;
TOTAL_SUM = 0;
%=============================================================================
comm = MPI_Comm_object('MPI_COMM_WORLD');
comm_rank = MPI_Comm_rank (comm);
comm_size = MPI_Comm_size(comm);
%=============================================================================
TAG = 1000 + comm_rank;
master = 0;
%=============================================================================
if (comm_rank == master)
  tic();
end
M = N * inv(comm_size);
R = N - M * floor(N * inv(M));
if (comm_rank == comm_size - 1)
  subsum = (comm_rank + 1) * M + R;
else
  subsum = (comm_rank + 1) * M;
end
%=============================================================================
S1 = TOTAL_SUM;
for i = (comm_rank * M) + 1 : subsum
  S1 = S1 + i;
end
disp(['Partial summation ', mat2str(S1), ' on process ', int2str(comm_rank)]);
%=============================================================================
if (comm_rank != master)
  MPI_Send(S1, master, TAG, comm);
else
  TOTAL_SUM = S1;
  range = [1: comm_size - 1];
  for source = range
    TAG = 1000 + source;
    s2 = MPI_Recv (source, TAG, comm);
    TOTAL_SUM = TOTAL_SUM + s2;
  end
end
%=============================================================================
if comm_rank == master
  disp(['Total summation: ', mat2str(TOTAL_SUM)]);
  disp(['Time: ', mat2str(toc())]);
end
%=============================================================================
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
exit
%=============================================================================
