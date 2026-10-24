%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function solver = odeSolverName(solver)
  if isstring(solver)
    solver = char(solver);
  end
  if ~ischar(solver)
    error('Nelson:ode_solvers:solverNameExpected', 'Solver name expected.');
  end
  allowed = {'auto', 'autoswitch', 'stiff', 'nonstiff', 'ode45', 'ode23', 'ode78', ...
    'ode89', 'ode113', 'ode15s', 'ode23s', 'ode23t', 'ode23tb', 'ode15i', ...
    'cvodesnonstiff', 'cvodesstiff', 'idas'};
  idx = find(strcmpi(solver, allowed));
  if isempty(idx)
    error('Nelson:ode_solvers:unknownOdeSolver', 'Unknown ODE solver.');
  end
  solver = allowed{idx(1)};
end
%=============================================================================
