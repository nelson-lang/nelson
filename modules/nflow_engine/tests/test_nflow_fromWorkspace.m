%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fromWorkspace block: matrix/struct formats, interpolation, extrapolation,
% duplicated-time discontinuities, and error handling. Expected values are
% analytic (ramps and steps with closed-form interpolants).
% Note: the engine enforces duration >= 10*sampleTime, so grids use >= 10 steps.
%=============================================================================
% Linear interpolation of a ramp valued 3*t, grid t = 0:0.1:1.
% The fromWorkspace block reads the BASE workspace, so the
% input variables must be assigned there for the test to pass under test_run.
assignin('base', 'simin', [(0:0.1:1)', (0:0.1:1)' * 3]);
r = run_fw('simin', 'on', 'Holding final value', 0.1, 1.0);
asserts.isequal(r.status, 'done');
y = fw_first_series(r);
asserts.isapprox(y(:)', 3 * (0:0.1:1), 0, 1e-10);
%=============================================================================
% ZOH (Interpolate off): value of the latest sample with time <= t.
assignin('base', 'simin', [0 0; 1 10; 2 30]);
r = run_fw('simin', 'off', 'Holding final value', 0.2, 2.0);
y = fw_first_series(r);
asserts.isequal(y(:)', [0 0 0 0 0 10 10 10 10 10 30]);
%=============================================================================
% Duplicated time = discontinuity; with interpolation the NEW value wins at t.
assignin('base', 'simin', [0 0; 1 10; 1 20; 2 30]);
r = run_fw('simin', 'on', 'Holding final value', 0.2, 2.0);
y = fw_first_series(r);
asserts.isapprox(y(:)', [0 2 4 6 8 20 22 24 26 28 30], 0, 1e-10);
%=============================================================================
% Extrapolation before the first point (data starts at t=1), interp on.
% Linear extrapolation from (1,10),(2,20): y = 10*t.
assignin('base', 'simin', [1 10; 2 20]);
r = run_fw('simin', 'on', 'Holding final value', 0.2, 2.0);
y = fw_first_series(r);
asserts.isapprox(y(:)', 10 * (0:0.2:2), 0, 1e-10);
%=============================================================================
% Struct-with-time format (time + signals.values), interp y = 10*t.
assignin('base', 's', struct('time', (0:0.5:1)', 'signals', struct('values', [0; 5; 10])));
r = run_fw('s', 'on', 'Holding final value', 0.1, 1.0);
y = fw_first_series(r);
asserts.isapprox(y(:)', 10 * (0:0.1:1), 0, 1e-10);
%=============================================================================
% Error: variable absent from the base workspace.
evalin('base', 'clear(''simin_absent'')');
r = run_fw('simin_absent', 'on', 'Extrapolation', 0.1, 1.0);
asserts.isequal(r.status, 'error');
asserts.istrue(~isempty(strfind(r.reason, 'not found')));
%=============================================================================
% Error: Extrapolation requested with Interpolate off.
assignin('base', 'simin', [0 0; 1 10]);
r = run_fw('simin', 'off', 'Extrapolation', 0.1, 1.0);
asserts.isequal(r.status, 'error');
asserts.istrue(~isempty(strfind(r.reason, 'interpolation is not enabled')));
%=============================================================================
% Error: non-monotonic time.
assignin('base', 'simin', [0 0; 2 1; 1 2]);
r = run_fw('simin', 'on', 'Holding final value', 0.1, 1.0);
asserts.isequal(r.status, 'error');
asserts.istrue(~isempty(strfind(r.reason, 'non-decreasing')));
%=============================================================================
function r = run_fw(varName, interpolate, afterFinal, dt, dur)
  d = struct();
  d.blocks = {struct('id', 'fw', 'type', 'fromWorkspace', 'inputs', 0, 'outputs', 1, ...
      'params', struct('VariableName', varName, 'Interpolate', interpolate, ...
      'OutputAfterFinalValue', afterFinal)), ...
    struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
  d.connections = {struct('from', 'fw', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = dt; d.duration = dur; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
end
%=============================================================================
function y = fw_first_series(r)
  y = r.scopes(1).series;
  if iscell(y), y = y{1}; else, y = y(1, :); end
end
%=============================================================================
