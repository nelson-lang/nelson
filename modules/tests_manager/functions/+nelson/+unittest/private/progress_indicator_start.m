%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function progress = progress_indicator_start(label, total, transient)
  if nargin < 3
    transient = false;
  end
  progress = struct('enabled', false, 'label', label, ...
    'total', max(1, double(total)), 'last_percent', -1, 'previous', '', ...
    'transient', logical(transient));
  if isempty(label) || ~progress_indicator_enabled(total)
    return
  end
  progress.enabled = true;
  progress = progress_indicator_update(progress, 0);
end
%=============================================================================
