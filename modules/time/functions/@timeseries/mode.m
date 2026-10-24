%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = mode(obj, varargin)
  data = obj.Data;
  if isempty(data)
    value = [];
    return
  end
  if nargin > 1
    dim = varargin{1};
  else
    dim = 1;
  end
  if dim ~= 1
    error('Nelson:time:firstDimensionModeSupportedTimeseries', 'Only first-dimension mode is supported for timeseries data.');
  end
  data = reshape(data, size(data, 1), []);
  value = zeros(1, size(data, 2));
  for col = 1:size(data, 2)
    values = unique(data(:, col));
    counts = zeros(size(values));
    for k = 1:length(values)
      counts(k) = sum(data(:, col) == values(k));
    end
    [~, idx] = max(counts);
    value(col) = values(idx);
  end
end
%=============================================================================
