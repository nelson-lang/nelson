%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = tableIsMissingValue(value, indicators)
  if nargin < 2
    indicators = {};
  end
  if iscell(value)
    tf = false(size(value, 1), size(value, 2));
    for k = 1:numel(value)
      tf(k) = tableIsMissingValue(value{k}, indicators);
    end
    return
  end
  if isstring(value)
    tf = value == "";
  elseif ischar(value)
    if isempty(value)
      tf = true;
    elseif isrow(value)
      tf = false;
    else
      tf = false(size(value, 1), 1);
      for r = 1:size(value, 1)
        tf(r) = isempty(deblank(value(r, :)));
      end
    end
  elseif isnumeric(value)
    if isfloat(value)
      tf = isnan(value);
    else
      tf = false(size(value, 1), size(value, 2));
    end
  elseif islogical(value)
    tf = false(size(value, 1), size(value, 2));
  else
    tf = false(size(value, 1), size(value, 2));
  end
  for k = 1:length(indicators)
    try
      tf = tf | (value == indicators{k});
    catch
      try
        tf = tf | strcmp(value, indicators{k});
      catch
      end
    end
  end
end
%=============================================================================
