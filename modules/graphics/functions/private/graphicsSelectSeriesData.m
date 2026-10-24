%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = graphicsSelectSeriesData(values, orientation, k, outputShape)
  if nargin < 4
    outputShape = 'row';
  end
  if strcmp(orientation, 'single')
    data = reshapeSeries(values, outputShape);
  elseif isscalar(values)
    data = values;
  elseif strcmp(orientation, 'points')
    data = values(k);
  elseif isvector(values)
    data = reshapeSeries(values, outputShape);
  elseif strcmp(orientation, 'columns')
    data = reshapeSeries(values(:, k), outputShape);
  elseif strcmp(outputShape, 'column')
    data = values(k, :).';
  else
    data = values(k, :);
  end
end
%=============================================================================
function data = reshapeSeries(values, outputShape)
  if strcmp(outputShape, 'column')
    data = values(:);
  else
    data = values(:).';
  end
end
%=============================================================================
