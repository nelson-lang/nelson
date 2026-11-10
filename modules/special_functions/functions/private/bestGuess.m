%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = bestGuess(residual, varargin)
  % keeps, for each element, the starting point with the smallest residual
  t = varargin{1};
  best = abs(residual(t, ':'));
  best(~isfinite(t)) = Inf;
  for k = 2:numel(varargin)
    candidate = varargin{k};
    r = abs(residual(candidate, ':'));
    r(~isfinite(candidate)) = Inf;
    better = r < best | isnan(best);
    t(better) = candidate(better);
    best(better) = r(better);
  end
  t(~isfinite(t)) = 0;
end
%=============================================================================
