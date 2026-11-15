%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function upper = parseTail(tail)
  % true for 'upper', false for 'lower', [] when tail is neither
  if (ischar(tail) || isstring(tail)) && any(strcmpi(char(tail), {'lower', 'upper'}))
    upper = strcmpi(char(tail), 'upper');
  else
    upper = [];
  end
end
%=============================================================================
