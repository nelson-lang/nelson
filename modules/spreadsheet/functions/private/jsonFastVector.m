%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [vector, ok] = jsonFastVector(rows, name)
  % Fast path for a struct array field holding one number or one logical
  % per row (no null, no text, no array): returns the n x 1 vector.
  vector = [];
  ok = false;
  n = numel(rows);
  if n == 0
    return
  end
  first = rows(1).(name);
  if ~(isnumeric(first) || islogical(first)) || ~isscalar(first)
    return
  end
  try
    vector = [rows.(name)];
  catch
    return
  end
  ok = (isnumeric(vector) || islogical(vector)) && numel(vector) == n;
  if ok
    vector = vector(:);
  else
    vector = [];
  end
end
%=============================================================================
