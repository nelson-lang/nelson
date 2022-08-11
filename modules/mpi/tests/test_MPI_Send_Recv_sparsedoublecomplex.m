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

A = reshape(1:12, 3, 4);
B = reshape(1:12, 3, 4);

C = sparse(complex(A, B));

TAG= 1;
if (my_rank != 0)
  rankvect = 0;
  
  MPI_Send(C, rankvect, TAG)
else
  for source = 1:num_ranks - 1
    receive = MPI_Recv (source, TAG);
    assert_istrue(isa(receive, 'sparsedouble'));
    assert_isequal(receive, C);
  end
end
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
