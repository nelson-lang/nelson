%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = isTruthyEnvironmentValue(name)
  value = getenv(name);
  if isempty(value)
    res = false;
    return
  end
  normalized = lower(strtrim(value));
  res = ~any(strcmp(normalized, {'0', 'false', 'no', 'off'}));
end
%=============================================================================
