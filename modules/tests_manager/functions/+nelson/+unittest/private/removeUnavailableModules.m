%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function modules_list = removeUnavailableModules(modules_list)
  if strcmp(getenv('AUDIODEV'), 'null')
    modules_list(modules_list == "audio") = [];
  end
  if strcmp(getenv('NELSON_WITHOUT_IPC'), 'TRUE')
    modules_list(modules_list == "ipc") = [];
  end
  if strcmp(getenv('NELSON_WITHOUT_MPI'), 'TRUE')
    modules_list(modules_list == "mpi") = [];
  end
  if ~ismodule('slicot')
    modules_list(modules_list == "slicot") = [];
  end
  if ~ismodule('mpi')
    modules_list(modules_list == "mpi") = [];
  end
end
