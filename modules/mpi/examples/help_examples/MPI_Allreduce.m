%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~MPI_Initialized()
  MPI_Init();
end
comm = MPI_Comm_object();
my_rank = MPI_Comm_rank ();
num_ranks = MPI_Comm_size();

A = [1 + my_rank:3 + my_rank]
B = MPI_Allreduce(A, 'MPI_PROD', comm);
if (my_rank == 0)
  disp('Result:')
  disp(B);
end
if MPI_Initialized()
  MPI_Finalize();
end
