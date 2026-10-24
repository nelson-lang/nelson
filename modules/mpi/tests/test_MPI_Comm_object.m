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
asserts.isequal(class(comm), 'MPI_Comm');
asserts.istrue(isvalid(comm));
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
