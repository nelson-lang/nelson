%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function res_struct = worker_pool_safe_runfile(filename, varargin)
  resetState = true;
  mpiMode = 0;
  if nargin > 1
    resetState = logical(varargin{1});
  end
  if nargin > 2
    mpiMode = varargin{2};
  end
  if resetState
    savedState = worker_pool_save_state();
    lasterror('reset');
    lastwarn('');
  end
  try
    res_struct = runfile(filename, mpiMode);
  catch worker_ex
    res_struct = struct('r', 1, 'msg', worker_ex.message, 'stdout', '', 'timing', 0);
  end
  if resetState
    % Restore through the builtin before entering another function: a test may
    % have lowered the limit below the runner's current call depth.
    max_recursion_depth(savedState.recursionDepth);
    worker_pool_restore_state(savedState);
  end
end
%=============================================================================
function savedState = worker_pool_save_state()
  savedState.path = path();
  savedState.directory = pwd();
  savedState.displayFormat = format();
  savedState.randomState = rng();
  savedState.recursionDepth = max_recursion_depth();
  savedState.warningState = warning();
  [savedState.warningMessage, savedState.warningIdentifier] = lastwarn();
end
%=============================================================================
