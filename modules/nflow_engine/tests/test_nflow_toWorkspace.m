%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% toWorkspace block: SaveFormat variants, Decimation, MaxDataPoints, and the
% fromWorkspace -> toWorkspace round-trip. Expected values are analytic
% (gain/slope chains with closed-form outputs).
%=============================================================================
% The toWorkspace block writes the BASE workspace, so results
% are read back with evalin('base', ...) to pass under test_run.
%=============================================================================
% Structure With Time: fields and analytic values (gain 2 * slope 3 = 6*t).
evalin('base', 'clear(''simout_test'')');
r = run_tw('Structure With Time', 1, 'inf', 0.1, 1.0);
asserts.isequal(r.status, 'done');
asserts.istrue(evalin('base', 'isvar(''simout_test'')'));
simout_test = evalin('base', 'simout_test');
asserts.istrue(isfield(simout_test, 'time'));
asserts.istrue(isfield(simout_test, 'signals'));
asserts.istrue(isfield(simout_test.signals, 'values'));
asserts.istrue(isfield(simout_test.signals, 'dimensions'));
asserts.istrue(isfield(simout_test, 'blockName'));
asserts.isapprox(simout_test.signals.values(:)', 6 * simout_test.time(:)', 0, 1e-10);
asserts.isequal(simout_test.blockName, 'tw');
%=============================================================================
% Array format: N-by-1 matrix, no time field.
evalin('base', 'clear(''simout_test'')');
r = run_tw('Array', 1, 'inf', 0.1, 1.0);
asserts.istrue(evalin('base', 'isvar(''simout_test'')'));
simout_test = evalin('base', 'simout_test');
asserts.istrue(isnumeric(simout_test));
asserts.isequal(size(simout_test, 1), 11);
%=============================================================================
% Structure format: empty time field.
evalin('base', 'clear(''simout_test'')');
r = run_tw('Structure', 1, 'inf', 0.1, 1.0);
simout_test = evalin('base', 'simout_test');
asserts.istrue(isempty(simout_test.time));
asserts.isequal(numel(simout_test.signals.values), 11);
%=============================================================================
% Decimation = 2: keep one sample out of two (indices 0,2,4,...).
evalin('base', 'clear(''simout_test'')');
r = run_tw('Structure With Time', 2, 'inf', 0.1, 1.0);
simout_test = evalin('base', 'simout_test');
asserts.isequal(numel(simout_test.time), 6);
asserts.isapprox(simout_test.time(:)', [0 0.2 0.4 0.6 0.8 1.0], 0, 1e-10);
%=============================================================================
% MaxDataPoints applied AFTER decimation: last N decimated samples.
evalin('base', 'clear(''simout_test'')');
r = run_tw('Structure With Time', 2, '3', 0.1, 1.0);
simout_test = evalin('base', 'simout_test');
asserts.isequal(numel(simout_test.time), 3);
asserts.isapprox(simout_test.time(:)', [0.6 0.8 1.0], 0, 1e-10);
%=============================================================================
function r = run_tw(saveFormat, decimation, maxPoints, dt, dur)
  simin = [(0:0.1:1)', (0:0.1:1)' * 3];
  assignin('base', 'simin', simin);
  d = struct();
  d.blocks = {struct('id', 'fw', 'type', 'fromWorkspace', 'inputs', 0, 'outputs', 1, ...
      'params', struct('VariableName', 'simin', 'Interpolate', 'on', ...
      'OutputAfterFinalValue', 'Holding final value')), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'simout_test', 'SaveFormat', saveFormat, ...
      'Decimation', decimation, 'MaxDataPoints', maxPoints))};
  d.connections = {struct('from', 'fw', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = dt; d.duration = dur; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
end
%=============================================================================
