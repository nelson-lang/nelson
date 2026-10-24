%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = timeseriesInterpolateData(obj, newTime, method)
  y = timeseriesDataAsColumns(obj);
  if isempty(y)
    data = [];
    return
  end
  if strcmpi(method, 'zoh')
    method = 'previous';
  end
  if strcmpi(method, 'previous')
    data = interpolatePrevious(obj.Time(:), y, newTime(:));
  else
    data = interp1(obj.Time(:), y, newTime(:), method, 'extrap');
  end
end
%=============================================================================
function out = interpolatePrevious(time, y, newTime)
  out = zeros(length(newTime), size(y, 2));
  for k = 1:length(newTime)
    idx = find(time <= newTime(k), 1, 'last');
    if isempty(idx)
      idx = 1;
    end
    out(k, :) = y(idx, :);
  end
end
%=============================================================================
