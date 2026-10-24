%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function idx = progressive_event_index(event)
  idx = event.index;
  while iscell(idx) || isstruct(idx)
    if isempty(idx)
      idx = 0;
      return
    end
    if iscell(idx)
      idx = idx{1};
    elseif isfield(idx, 'index')
      idx = idx(1).index;
    else
      idx = 0;
      return
    end
  end
  if isempty(idx)
    idx = 0;
  elseif length(idx) > 1
    idx = idx(1);
  end
end
%=============================================================================
