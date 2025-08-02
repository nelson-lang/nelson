%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if (exist('MPI_Comm_used') ~= 0)
  delete(MPI_Comm_used());
  removegateway(modulepath('mpi', 'builtin'));
end
if (exist('MPI_exec') ~= 0)
  rmpath(modulepath('mpi', 'functions'));
end
%=============================================================================