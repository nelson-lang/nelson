%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, yp] = deval(varargin)
  narginchk(2, 3);
  [sol, x, idx] = odeDevalArguments(varargin{:});
  data = odeSolutionData(sol);
  if isfield(data, 'extdata') && isfield(data.extdata, 'internal')
    data = data.extdata.internal;
  end
  t = data.x(:)';
  values = data.y;
  slopes = [];
  if isfield(data, 'extdata') && isfield(data.extdata, 'slopes')
    slopes = data.extdata.slopes;
  elseif isfield(data, 'yp')
    slopes = data.yp;
  end
  if isempty(t)
    y = [];
    yp = [];
    return
  end
  idx = odeDevalComponents(idx, size(values, 1));
  xrow = x(:)';
  odeValidateEvaluationPoints(t, xrow);
  [y, yp] = odeDevalInterpolate(t, values, xrow, slopes);
  if ~isempty(idx)
    y = y(idx, :);
    yp = yp(idx, :);
  end
end
%=============================================================================
function [sol, x, idx] = odeDevalArguments(varargin)
  if odeIsSolutionArgument(varargin{1})
    sol = varargin{1};
    x = varargin{2};
  elseif odeIsSolutionArgument(varargin{2})
    sol = varargin{2};
    x = varargin{1};
  else
    error('Nelson:ode_solvers:odeSolutionStructureExpected', 'ODE solution structure expected.');
  end
  if ~isnumeric(x)
    error('Nelson:ode_solvers:evaluationPointsNumeric', 'Evaluation points must be numeric.');
  end
  idx = [];
  if nargin == 3
    idx = varargin{3};
  end
end
%=============================================================================
function tf = odeIsSolutionArgument(value)
  tf = isa(value, 'nelson.ode.ODEResults') ...
    || (isstruct(value) && isfield(value, 'x') && isfield(value, 'y'));
end
%=============================================================================
function idx = odeDevalComponents(idx, componentCount)
  if isempty(idx)
    return
  end
  if ~isnumeric(idx) || ~isvector(idx) || any(idx(:) < 1) ...
      || any(idx(:) > componentCount) || any(fix(idx(:)) ~= idx(:))
    error('Nelson:ode_solvers:componentIndicesInvalid', '%s indices are invalid.', 'Component');
  end
  idx = idx(:);
end
%=============================================================================
function odeValidateEvaluationPoints(t, x)
  forward = t(length(t)) >= t(1);
  if forward
    invalid = any(x < t(1) | x > t(length(t)));
  else
    invalid = any(x > t(1) | x < t(length(t)));
  end
  if invalid
    error('Nelson:ode_solvers:evaluationPointsLieInsideSolutionInterval', 'Evaluation points must lie inside the solution interval.');
  end
end
%=============================================================================
