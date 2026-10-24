%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = detrend(x, o, bp)
  narginchk(1, 3);
  if nargin < 2 || isempty(o)
    o = 1;
  end
  if ischar(o) || (isstring(o) && isscalar(o))
    switch lower(char(o))
      case 'constant'
        o = 0;
      case 'linear'
        o = 1;
      otherwise
        error('Nelson:data_analysis:trendTypeConstantLinear', 'Trend type must be ''constant'' or ''linear''.');
    end
  end
  if nargin < 3
    bp = [];
  end
  isRowVector = (ndims(x) == 2) && (size(x, 1) == 1);
  if isRowVector
    x = x(:);
  end
  N = size(x, 1);
  if o == 0
    y = x - mean(x, 1);
  else
    breaks = unique([0; double(bp(:)); N - 1]);
    lastSegment = length(breaks) - 1;
    a = [zeros(N, lastSegment, class(x)), ones(N, 1, class(x))];
    for kb = 1:lastSegment
      segmentLength = N - breaks(kb);
      a((1:segmentLength) + breaks(kb), kb) = (1:segmentLength)' / segmentLength;
    end
    y = x - a * (a \ x);
  end
  if isRowVector
    y = y.';
  end
end
%=============================================================================
