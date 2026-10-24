%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function H = optimFminconInitialHessian(x, opts)
  n = length(x);
  if strcmpi(opts.ScaleProblem, 'none')
    H = eye(n);
    return
  end
  if isempty(opts.TypicalX)
    scale = max(1, abs(x(:)));
  elseif isscalar(opts.TypicalX)
    scale = abs(double(opts.TypicalX)) * ones(n, 1);
  else
    scale = abs(double(opts.TypicalX(:)));
  end
  if length(scale) ~= n
    scale = ones(n, 1);
  end
  scale(~isfinite(scale) | scale < sqrt(eps)) = 1;
  diagonal = 2 ./ (scale(:) .^ 2);
  diagonal = min(max(diagonal, 1e-12), 1e12);
  H = diag(diagonal);
end
%=============================================================================
