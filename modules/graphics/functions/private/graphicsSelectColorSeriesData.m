%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = graphicsSelectColorSeriesData(values, orientation, k, nSeries, nPoints, outputShape)
  if nargin < 6
    outputShape = 'row';
  end
  data = values;
  if isempty(values) || graphicsIsRgbTriplet(values)
    return
  end
  if strcmp(orientation, 'single') && ismatrix(values) ...
      && isequal(size(values, 2), 3) && isequal(size(values, 1), nPoints)
    data = values;
  elseif ismatrix(values) && isequal(size(values, 2), 3) && isequal(size(values, 1), nSeries)
    data = values(k, :);
  elseif ismatrix(values) && numel(values) ~= nPoints
    data = graphicsSelectSeriesData(values, orientation, k, outputShape);
  end
end
%=============================================================================
