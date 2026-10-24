%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function residual = bvpResidual(odefun, bcfun, x, u, n, np, options)
  if nargin < 7
    options = [];
  end
  [y, parameters] = bvpResidualUnpack(u, n, length(x), np);
  residual = [];
  for k = 1:(length(x) - 1)
    h = x(k + 1) - x(k);
    f0 = bvpResidualOdeCall(odefun, x(k), y(:, k), parameters, options);
    f1 = bvpResidualOdeCall(odefun, x(k + 1), y(:, k + 1), parameters, options);
    midY = 0.5 * (y(:, k) + y(:, k + 1)) - h * (f1 - f0) / 8;
    fm = bvpResidualOdeCall(odefun, 0.5 * (x(k) + x(k + 1)), midY, parameters, options);
    residual = [residual; y(:, k + 1) - y(:, k) - h * (f0 + 4 * fm + f1) / 6];
  end
  residual = [residual; bvpResidualBcCall(bcfun, y(:, 1), y(:, length(x)), parameters)];
end
%=============================================================================
function [y, parameters] = bvpResidualUnpack(u, n, m, np)
  y = reshape(u(1:n * m), n, m);
  parameters = u(n * m + (1:np)).';
end
%=============================================================================
function values = bvpResidualOdeCall(fcn, x, y, parameters, options)
  if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
    values = feval(fcn, x, y);
  else
    values = feval(fcn, x, y, parameters);
  end
  values = values(:);
  if ~isempty(options)
    values = values + bvpSingularValue(options, x, y);
  end
end
%=============================================================================
function values = bvpResidualBcCall(fcn, ya, yb, parameters)
  if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
    values = feval(fcn, ya, yb);
  else
    values = feval(fcn, ya, yb, parameters);
  end
  values = values(:);
end
%=============================================================================
function value = bvpSingularValue(options, x, y)
  value = zeros(size(y));
  singularTerm = bvpget(options, 'SingularTerm', []);
  if isempty(singularTerm)
    return
  end
  if ~isnumeric(singularTerm) || ~isreal(singularTerm) ...
      || size(singularTerm, 1) ~= length(y) || size(singularTerm, 2) ~= length(y)
    error('Nelson:ode_solvers:singulartermRealSquareMatrixMatchingState', 'SingularTerm must be a real square matrix matching the state size.');
  end
  denominator = x;
  product = singularTerm * y;
  if abs(denominator) <= eps
    if max(abs(product)) > sqrt(eps)
      error('Nelson:ode_solvers:singulartermRequiresConsistentLeftBoundaryValue', 'SingularTerm requires a consistent left boundary value.');
    end
    return
  end
  value = product / denominator;
end
%=============================================================================
