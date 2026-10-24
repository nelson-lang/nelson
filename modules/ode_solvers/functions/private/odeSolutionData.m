%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = odeSolutionData(sol)
  if isa(sol, 'nelson.ode.ODEResults')
    sol = sol.RawSolution;
  end
  if ~isstruct(sol) || ~isfield(sol, 'x') || ~isfield(sol, 'y')
    error('Nelson:ode_solvers:odeSolutionStructureExpected', 'ODE solution structure expected.');
  end
  if ~isfield(sol, 'xe')
    sol.xe = [];
  end
  if ~isfield(sol, 'ye')
    sol.ye = zeros(size(sol.y, 1), 0);
  end
  if ~isfield(sol, 'ie')
    sol.ie = [];
  end
  if ~isfield(sol, 'stats')
    sol.stats = odeInitialStats();
  end
  data = sol;
end
%=============================================================================
