%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_resize3_linear_dim(data, outputLength, dim)
  inputLength = size(data, dim);
  [lo, hi, alpha] = ip_resize3_coordinates(inputLength, outputLength);
  if dim == 1
    weightShape = [outputLength 1 1];
    alpha = reshape(alpha, weightShape);
    out = data(lo, :, :) .* (1 - alpha) + data(hi, :, :) .* alpha;
  elseif dim == 2
    weightShape = [1 outputLength 1];
    alpha = reshape(alpha, weightShape);
    out = data(:, lo, :) .* (1 - alpha) + data(:, hi, :) .* alpha;
  else
    weightShape = [1 1 outputLength];
    alpha = reshape(alpha, weightShape);
    out = data(:, :, lo) .* (1 - alpha) + data(:, :, hi) .* alpha;
  end
end
%=============================================================================
function [lo, hi, alpha] = ip_resize3_coordinates(inputLength, outputLength)
  coord = ((1:outputLength) - 0.5) * inputLength / outputLength + 0.5;
  lo = floor(coord);
  hi = lo + 1;
  alpha = coord - lo;
  lo = min(max(lo, 1), inputLength);
  hi = min(max(hi, 1), inputLength);
end
%=============================================================================
