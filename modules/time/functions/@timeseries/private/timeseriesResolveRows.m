%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rows = timeseriesResolveRows(obj, selector)
  if ischar(selector) && strcmp(selector, ':')
    rows = 1:obj.Length;
  elseif islogical(selector)
    rows = find(selector(:)).';
  else
    rows = selector;
  end
end
%=============================================================================
