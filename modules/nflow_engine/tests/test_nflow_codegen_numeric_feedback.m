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
% <--WEIGHT 64-->
%=============================================================================
% Numeric equivalence of GENERATED code vs the interpreter for a unity-feedback
% control loop (P / PI / PID controller driving a strictly proper tf plant), for
% BOTH targets. This guards the codegen cycle-break (latch) selection: a feedback
% loop must be broken at a block with NO real feedthrough. A strictly proper tf
% plant is the correct break point (it reads purely from state); the pid
% controller is NOT (its P and D terms feed through the current input). A prior
% version trusted the coarse static directFeedthrough flag (tf marked feedthrough
% to cover the pure-gain case, pid marked non-feedthrough for scheduling parity),
% so it broke the loop at the pid and emitted its output from a stale, zero
% input: the pure-P path collapsed to exactly 0 for the whole run and the PI/PID
% paths followed a wrong trajectory. blockHasCodegenFeedthrough resolves the
% feedthrough per instance, so the loop now breaks at the tf plant.
%
% The reference uses the default solver: the interpreter's explicit fixed-step
% backends do not sample a to-workspace sink that sits inside a feedback cycle
% (they read it before the cycle resolves), so only the default path yields a
% faithful reference trajectory here. The generated program integrates with its
% own fused fixed step, so the tolerance is looser than the tight fixed-step-vs-
% fixed-step basic harness; it still separates the fixed behaviour (a few percent
% off the reference) from the regression (the pure-P plant frozen at 0, the PI /
% PID plants far off) by more than an order of magnitude.
%=============================================================================
skip_testsuite(~__nflow_codegen_toolchain__('C'), 'C toolchain (cmake) not available');
%=============================================================================
function b = feedback_loop(tag, P, I, D)
  % One unity-feedback loop: sum(+-) -> pid -> tf plant -> back to the sum.
  % The plant output is exported (labelSink, the generated CSV column) and
  % logged (toWorkspace, the interpreter reference).
  b = {...
    struct('id', ['e_', tag], 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
      'params', struct('Inputs', '+-')), ...
    struct('id', ['c_', tag], 'type', 'pid', 'inputs', 1, 'outputs', 1, ...
      'params', struct('P', P, 'I', I, 'D', D)), ...
    struct('id', ['p_', tag], 'type', 'tf', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Numerator', [1], 'Denominator', [1, 0.5, 1])), ...
    struct('id', ['lp_', tag], 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('GotoTag', ['plant_', tag], 'isExternalPort', true)), ...
    struct('id', ['tp_', tag], 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', ['ref_plant_', tag], 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
end
%=============================================================================
function c = feedback_wiring(tag)
  c = {...
    struct('from', 'r', 'to', ['e_', tag], 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', ['p_', tag], 'to', ['e_', tag], 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', ['e_', tag], 'to', ['c_', tag], 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', ['c_', tag], 'to', ['p_', tag], 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', ['p_', tag], 'to', ['lp_', tag], 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', ['p_', tag], 'to', ['tp_', tag], 'fromIndex', 0, 'toIndex', 0)};
end
%=============================================================================
% Three parallel loops sharing one step setpoint: pure P, PI and full PID,
% mirroring the shipped PID_Demo example.
d = struct();
d.blocks = [...
  {struct('id', 'r', 'type', 'step', 'inputs', 0, 'outputs', 1, 'params', struct('Time', 1))}, ...
  feedback_loop('p', 2, 0, 0), ...
  feedback_loop('pi', 2, 1, 0), ...
  feedback_loop('pid', 3, 1.5, 1.2)];
d.connections = [feedback_wiring('p'), feedback_wiring('pi'), feedback_wiring('pid')];
d.sampleTime = 0.01;
d.duration = 8.0;
d.variables = struct();
outNames = {'plant_p', 'plant_pi', 'plant_pid'};
refVars = {'ref_plant_p', 'ref_plant_pi', 'ref_plant_pid'};
%=============================================================================
nflow_codegen_check_numeric(d, 'nfcg_feedback', 'C', outNames, refVars, 0.06, 0.06);
%=============================================================================
