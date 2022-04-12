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
if ~MPI_Initialized()
  MPI_Init();
end
my_rank = MPI_Comm_rank ();
num_ranks = MPI_Comm_size();

TAG= 1;
A = int32(eye(3, 4))
if (my_rank != 0)
  rankvect = 0;
  MPI_Send(A, rankvect, TAG)
else
  for source = 1:num_ranks - 1
    receive = MPI_Recv (source, TAG);
    assert_istrue(isa(receive, 'int32'));
    assert_isequal(receive, A)
  end
end
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
