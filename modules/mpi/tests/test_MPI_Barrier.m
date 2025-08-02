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
if ~MPI_Initialized()
  MPI_Init();
end
my_rank = MPI_Comm_rank ();
num_ranks = MPI_Comm_size();
comm = MPI_Comm_object('MPI_COMM_WORLD');
MPI_Barrier(comm);
disp(['I am ', int2str(my_rank), ' of ', int2str(num_ranks)]);
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
