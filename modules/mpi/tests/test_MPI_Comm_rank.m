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
rank = MPI_Comm_rank();
asserts.istrue(rank >= 0);
asserts.istrue(rank < MPI_Comm_size());
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
