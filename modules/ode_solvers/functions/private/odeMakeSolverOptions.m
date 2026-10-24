%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = odeMakeSolverOptions(solver, varargin)
  solver = odeSolverOptionName(solver);
  switch solver
    case 'ode23'
      obj = nelson.ode.options.ODE23(varargin{:});
    case 'ode78'
      obj = nelson.ode.options.ODE78(varargin{:});
    case 'ode89'
      obj = nelson.ode.options.ODE89(varargin{:});
    case 'ode113'
      obj = nelson.ode.options.ODE113(varargin{:});
    case 'ode15s'
      obj = nelson.ode.options.ODE15s(varargin{:});
    case 'ode23s'
      obj = nelson.ode.options.ODE23s(varargin{:});
    case 'ode23t'
      obj = nelson.ode.options.ODE23t(varargin{:});
    case 'ode23tb'
      obj = nelson.ode.options.ODE23tb(varargin{:});
    case 'ode15i'
      obj = nelson.ode.options.ODE15i(varargin{:});
    case {'autoswitch', 'cvodesnonstiff'}
      obj = nelson.ode.options.CVODESNonstiff(varargin{:});
    case 'cvodesstiff'
      obj = nelson.ode.options.CVODESStiff(varargin{:});
    case 'idas'
      obj = nelson.ode.options.IDAS(varargin{:});
    otherwise
      obj = nelson.ode.options.ODE45(varargin{:});
  end
end
%=============================================================================
function solver = odeSolverOptionName(solver)
  solver = odeSolverName(solver);
  allowed = {'autoswitch', 'ode45', 'ode23', 'ode78', 'ode89', 'ode113', 'ode15s', ...
    'ode23s', 'ode23t', 'ode23tb', 'ode15i', 'cvodesnonstiff', ...
    'cvodesstiff', 'idas'};
  if ~any(strcmp(solver, allowed))
    error('Nelson:ode_solvers:unknownOdeSolver', 'Unknown ODE solver.');
  end
end
%=============================================================================
