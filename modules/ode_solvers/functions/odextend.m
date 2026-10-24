%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function solout = odextend(sol, odefun, tfinal, options)
  narginchk(3, 4);
  data = odeSolutionData(sol);
  if nargin < 4 || isempty(options)
    options = data.extdata.options;
    options = odeReusableOptions(options);
  end
  if isempty(odefun)
    odefun = data.extdata.odefun;
  end
  lastIndex = length(data.x);
  if odeIsTimeInSolutionInterval(data.x, tfinal)
    warning('Nelson:odextend:solutionAlreadyAvailable', 'Solution already available. Use deval.');
    solout = odeRemoveEmptyEventFields(data);
    return
  end
  if ~odeCanExtendTo(data.x, tfinal)
    error('Nelson:ode_solvers:odeSolutionCannotExtendedRequestedDirection', 'ODE solution cannot be extended in the requested direction.');
  end
  y0 = data.y(:, size(data.y, 2));
  tspan = [data.x(lastIndex), tfinal];
  extended = odeRunSolver(data.solver, odefun, tspan, y0, [], options);
  solout = odeAppendSolutions(data, extended);
end
%=============================================================================
function options = odeReusableOptions(options)
  if isstruct(options) && isfield(options, 'MinStep') && isequal(options.MinStep, 0)
    options.MinStep = [];
  end
end
%=============================================================================
function tf = odeIsTimeInSolutionInterval(x, tfinal)
  direction = sign(x(length(x)) - x(1));
  if direction >= 0
    tf = tfinal >= x(1) && tfinal <= x(length(x));
  else
    tf = tfinal <= x(1) && tfinal >= x(length(x));
  end
end
%=============================================================================
function tf = odeCanExtendTo(x, tfinal)
  direction = sign(x(length(x)) - x(1));
  tf = direction * (tfinal - x(length(x))) > 0;
end
%=============================================================================
function sol = odeRemoveEmptyEventFields(sol)
  if isfield(sol, 'xe') && isempty(sol.xe)
    sol = rmfield(sol, {'xe', 'ye', 'ie'});
  end
end
%=============================================================================
