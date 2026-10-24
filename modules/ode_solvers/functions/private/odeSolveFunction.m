%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = odeSolveFunction(solver, nout, varargin)
  if strcmp(solver, 'ode15i')
    [odefun, tspan, y0, yp0, options] = odeParse15i(varargin{:});
  else
    [odefun, tspan, y0, options] = odeParseStandard(varargin{:});
    yp0 = [];
  end
  sol = odeRunSolver(solver, odefun, tspan, y0, yp0, options, nout);
  if nout <= 1
    varargout{1} = sol;
    return
  end
  if ~odeHasRequestedOutputOnly(sol)
    sol = odeResampleSolution(sol, tspan);
  end
  data = odeSolutionData(sol);
  varargout{1} = data.x(:);
  varargout{2} = data.y.';
  if nout >= 3
    varargout{3} = data.xe(:);
  end
  if nout >= 4
    varargout{4} = data.ye.';
  end
  if nout >= 5
    varargout{5} = data.ie(:);
  end
end
%=============================================================================
function tf = odeHasRequestedOutputOnly(sol)
  tf = isstruct(sol) && isfield(sol, 'extdata') && isfield(sol.extdata, 'requestedOutputOnly') ...
    && sol.extdata.requestedOutputOnly;
end
%=============================================================================
function [odefun, tspan, y0, options] = odeParseStandard(varargin)
  narginchk(3, inf);
  odefun = varargin{1};
  tspan = varargin{2};
  y0 = varargin{3};
  if nargin < 4
    options = [];
  else
    options = varargin{4};
  end
  if nargin > 4
    options = odeAttachParameters(options, varargin(5:end));
  end
end
%=============================================================================
function [odefun, tspan, y0, yp0, options] = odeParse15i(varargin)
  narginchk(4, inf);
  odefun = varargin{1};
  tspan = varargin{2};
  y0 = varargin{3};
  yp0 = varargin{4};
  if nargin < 5
    options = [];
  else
    options = varargin{5};
  end
  if nargin > 5
    options = odeAttachParameters(options, varargin(6:end));
  end
end
%=============================================================================
function options = odeAttachParameters(options, parameters)
  if isempty(options)
    options = struct();
  elseif odeIsSolverOptions(options)
    options = struct(options);
  elseif ~isstruct(options)
    error('Nelson:ode_solvers:optionsArgumentStructureCreatedOdeset', 'Options argument must be a structure created with odeset.');
  end
  options.Parameters = parameters;
end
%=============================================================================
