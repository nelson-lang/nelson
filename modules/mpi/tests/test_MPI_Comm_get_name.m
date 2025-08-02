%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
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
comm = MPI_Comm_object();
assert_isequal(MPI_Comm_get_name(comm), 'MPI_COMM_WORLD');
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
