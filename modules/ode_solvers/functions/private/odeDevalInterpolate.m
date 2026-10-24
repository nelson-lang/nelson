%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, yp] = odeDevalInterpolate(t, values, x, slopes)
  if nargin < 4
    slopes = [];
  end
  if odeCanUseNativeDeval(t, values, x, slopes)
    [y, yp] = __ode_deval_interpolate__(t, values, x, slopes);
    return
  end
  y = zeros(size(values, 1), length(x));
  yp = zeros(size(values, 1), length(x));
  for k = 1:length(x)
    [y(:, k), yp(:, k)] = odeLinearInterpolate(t, values, x(k), slopes);
  end
end
%=============================================================================
function tf = odeCanUseNativeDeval(t, values, x, slopes)
  persistent hasNative;
  if isempty(hasNative)
    hasNative = exist('__ode_deval_interpolate__') == 5;
  end
  tf = hasNative && isa(t, 'double') && isa(values, 'double') && isa(x, 'double') ...
    && isreal(t) && isreal(values) && isreal(x) ...
    && (isempty(slopes) || (isa(slopes, 'double') && isreal(slopes)));
end
%=============================================================================
