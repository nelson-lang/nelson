%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function solinit = bvpinit(xinit, yinit, parameters)
  narginchk(2, 3);
  if nargin < 3
    parameters = [];
  end
  x = bvpValidateMesh(xinit);
  y = bvpInitialValues(x, yinit);
  solinit = struct('x', x, 'y', y, 'parameters', parameters(:).');
end
%=============================================================================
function x = bvpValidateMesh(xinit)
  if ~isnumeric(xinit) || numel(xinit) < 2 || any(~isfinite(xinit(:)))
    error('Nelson:ode_solvers:initialMeshContainLeastTwoFinite', 'Initial mesh must contain at least two finite numeric values.');
  end
  x = xinit(:).';
  if any(diff(x) <= 0)
    error('Nelson:ode_solvers:initialMeshStrictlyIncreasing', 'Initial mesh must be strictly increasing.');
  end
end
%=============================================================================
function y = bvpInitialValues(x, yinit)
  if isa(yinit, 'function_handle')
    first = feval(yinit, x(1));
    y = zeros(numel(first(:)), length(x));
    y(:, 1) = first(:);
    for k = 2:length(x)
      y(:, k) = feval(yinit, x(k));
    end
  elseif isnumeric(yinit)
    y = bvpNumericInitialValues(x, yinit);
  else
    error('Nelson:ode_solvers:initialGuessNumericFunctionHandle', '%s must be numeric or a function handle.', 'Initial guess');
  end
end
%=============================================================================
function y = bvpNumericInitialValues(x, yinit)
  if isempty(yinit) || any(~isfinite(yinit(:)))
    error('Nelson:ode_solvers:initialGuessContainFiniteNumericValues', 'Initial guess must contain finite numeric values.');
  end
  if isvector(yinit)
    y = repmat(yinit(:), 1, length(x));
  elseif size(yinit, 2) == length(x)
    y = yinit;
  else
    error('Nelson:ode_solvers:initialGuessMatrixOneColumnPer', 'Initial guess matrix must have one column per mesh point.');
  end
end
%=============================================================================
