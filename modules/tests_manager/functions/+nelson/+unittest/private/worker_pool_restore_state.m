%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function worker_pool_restore_state(savedState)
  path(savedState.path);
  if isdir(savedState.directory)
    cd(savedState.directory);
  end
  format(savedState.displayFormat);
  rng(savedState.randomState);
  lasterror('reset');
  lasterr('', '');
  clear('functions');
  warning(savedState.warningState);
  lastwarn(savedState.warningMessage, savedState.warningIdentifier);
end
%=============================================================================
