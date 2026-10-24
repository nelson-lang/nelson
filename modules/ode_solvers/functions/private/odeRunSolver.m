%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeRunSolver(solver, odefun, tspan, y0, yp0, options, nout)
  if nargin < 7
    nout = 1;
  end
  if strcmp(solver, 'autoswitch')
    sol = odeRunAutoSwitchSolver(odefun, tspan, y0, yp0, options, nout);
    return
  end
  problem = odePrepareProblem(solver, odefun, tspan, y0, yp0, options);
  problem.storeRequestedOutputOnly = nout > 1 && length(problem.tspan) > 2;
  if odeIsSundialsSolver(problem.solver)
    if ~odeSundialsAvailable()
      error('Nelson:ode_solvers:sundialsBackendNotAvailable', 'SUNDIALS backend is not available.');
    end
    sol = __ode_sundials_run__(problem);
    sol = odeApplySundialsOutputFcn(problem, sol);
    odeDisplayStats(problem, sol.stats);
    return
  end
  [x, y, slopes, xe, ye, ie, stats] = odeIntegrate(problem);
  sol = odeBuildSolution(problem, x, y, slopes, xe, ye, ie, stats);
  odeDisplayStats(problem, stats);
end
%=============================================================================
function sol = odeApplySundialsOutputFcn(problem, sol)
  if isempty(problem.options.OutputFcn)
    return
  end
  stop = odeCallOutput(problem, problem.tspan, problem.y0, 'init');
  stopIndex = [];
  if stop
    stopIndex = 1;
  else
    for k = 2:length(sol.x)
      stop = odeCallOutput(problem, sol.x(k), sol.y(:, k), '');
      if stop
        stopIndex = k;
        break
      end
    end
  end
  odeCallOutput(problem, [], [], 'done');
  if ~isempty(stopIndex)
    sol = odeTrimSundialsSolution(sol, stopIndex, problem.direction);
  end
end
%=============================================================================
function sol = odeTrimSundialsSolution(sol, stopIndex, direction)
  sol.x = sol.x(1:stopIndex);
  sol.y = sol.y(:, 1:stopIndex);
  if isfield(sol, 'yp')
    sol.yp = sol.yp(:, 1:stopIndex);
  end
  if isfield(sol, 'extdata') && isfield(sol.extdata, 'slopes')
    sol.extdata.slopes = sol.extdata.slopes(:, 1:stopIndex);
  end
  if isfield(sol, 'xe')
    if direction > 0
      keep = sol.xe <= sol.x(end);
    else
      keep = sol.xe >= sol.x(end);
    end
    sol.xe = sol.xe(keep);
    sol.ye = sol.ye(:, keep);
    sol.ie = sol.ie(keep);
  end
end
%=============================================================================
function sol = odeRunAutoSwitchSolver(odefun, tspan, y0, yp0, options, nout)
  nonstiffSolver = 'ode45';
  stiffSolver = 'ode15s';
  problem = odePrepareProblem(nonstiffSolver, odefun, tspan, y0, yp0, options);
  problem.storeRequestedOutputOnly = nout > 1 && length(problem.tspan) > 2;
  problem.autoSwitch = odeAutoSwitchState(nonstiffSolver, stiffSolver);
  [x, y, slopes, xe, ye, ie, stats] = odeIntegrate(problem);
  sol = odeBuildSolution(problem, x, y, slopes, xe, ye, ie, stats);
  sol = odeAnnotateAutoSwitch(sol, stats);
  odeDisplayStats(problem, stats);
end
%=============================================================================
function state = odeAutoSwitchState(nonstiffSolver, stiffSolver)
  state = struct();
  state.enabled = true;
  state.initialSolver = nonstiffSolver;
  state.stiffSolver = stiffSolver;
  state.selectedSolver = nonstiffSolver;
  state.switched = false;
  state.reason = 'nonstiffAccepted';
  state.switchTime = [];
  state.switchCount = 0;
  state.nonstiffCandidateSteps = 0;
  state.mode = 'step';
end
%=============================================================================
function sol = odeAnnotateAutoSwitch(sol, stats)
  sol.solver = stats.autoSwitchSelectedSolver;
  if isfield(sol, 'stats')
    sol.stats.autoSwitchInitialSolver = stats.autoSwitchInitialSolver;
    sol.stats.autoSwitchSelectedSolver = stats.autoSwitchSelectedSolver;
    sol.stats.autoSwitchSwitched = stats.autoSwitchSwitched;
    sol.stats.autoSwitchReason = stats.autoSwitchReason;
    sol.stats.autoSwitchLastReason = stats.autoSwitchLastReason;
    sol.stats.autoSwitchMode = stats.autoSwitchMode;
    sol.stats.autoSwitchSwitchTime = stats.autoSwitchSwitchTime;
    sol.stats.autoSwitchSwitchCount = stats.autoSwitchSwitchCount;
  end
end
%=============================================================================
