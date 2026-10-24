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
comm = MPI_Comm_object();
A = [1 2 3];
B = MPI_Allreduce(A, 'MPI_SUM', comm);
asserts.isequal(B, A .* MPI_Comm_size(comm));
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
