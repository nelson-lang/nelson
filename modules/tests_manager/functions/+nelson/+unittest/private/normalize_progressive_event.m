%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function event = normalize_progressive_event(event)
  fields = fieldnames(event);
  for k = 1:length(fields)
    name = fields{k};
    value = event.(name);
    event.(name) = unwrap_progressive_scalar(value);
  end
end
%=============================================================================
function value = unwrap_progressive_scalar(value)
  while iscell(value) && length(value) == 1
    value = value{1};
  end
end
%=============================================================================
