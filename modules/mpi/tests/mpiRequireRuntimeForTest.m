%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mpiRequireRuntimeForTest()
  if MPI_Initialized()
    return
  end
  try
    MPI_Init();
  catch
    skip_testsuite(true, 'MPI runtime not available')
  end
end
%=============================================================================
