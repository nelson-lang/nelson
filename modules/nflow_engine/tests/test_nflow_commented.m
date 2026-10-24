%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Commented state honored in simulation: const(5) -> gain(2) -> W.
% Baseline = 10; gain Commented 'on' -> the block is removed (W reads 0);
% Commented 'through' -> the block is bypassed (the constant passes to W).
%=============================================================================
function y = run_commented(commented)
  d = struct();
  gainParams = struct('Gain', 2);
  if ~isempty(commented)
    gainParams.Commented = commented;
  end
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', 5)), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', gainParams), ...
    struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'cm_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'c', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.5; d.solver = 'discrete'; d.variables = struct();
  evalin('base', 'clear(''cm_out'')');
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(r.status, 'done');
  y = evalin('base', 'cm_out'); y = y(:)';
  evalin('base', 'clear(''cm_out'')');
end

% Baseline: gain applies -> 10.
yb = run_commented('');
assert_istrue(max(abs(yb - 10)) < 1e-9);

% Commented 'on': gain removed -> W input unconnected -> 0.
yon = run_commented('on');
assert_istrue(max(abs(yon - 0)) < 1e-9);

% Commented 'through': gain bypassed -> constant 5 reaches W.
yt = run_commented('through');
assert_istrue(max(abs(yt - 5)) < 1e-9);
%=============================================================================
