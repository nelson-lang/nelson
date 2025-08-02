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
root = 0;
if (my_rank == 0)
  buff = 777;
else
  buff = 0;
end
disp(['rank: ', int2str(my_rank), ': before Bcast, buff is ', int2str(buff)])
buff = MPI_Bcast(buff, root);
disp(['rank: ', int2str(my_rank), ': after Bcast, buff is ', int2str(buff)])
assert_isequal(buff, 777);
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
