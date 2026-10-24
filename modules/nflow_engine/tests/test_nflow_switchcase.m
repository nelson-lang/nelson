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
% L3: switchCase block. An integer control routes 1.0 to the matching case's
% output port and 0.0 to the others (default port on no match). These outputs
% are meant to gate action subsystems.
%=============================================================================

function y = run_switchcase(ctrl, port)
  d = struct();
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', ctrl)), ...
    struct('id', 'sw', 'type', 'switchCase', 'inputs', 1, 'outputs', 3, ...
      'params', struct('CaseConditions', '{1, [7 9 4]}', 'ShowDefaultCase', 'on')), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'sc_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'c', 'to', 'sw', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'sw', 'to', 'tw', 'fromIndex', port, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert(strcmp(r.status, 'done'));
  o = evalin('base', 'sc_out');
  y = o(end);
end

evalin('base', 'clear(''sc_out'')');
% ctrl = 9 matches case 1 (list [7 9 4]): port 1 is 1.0, ports 0 and 2 are 0.0.
asserts.isapprox(run_switchcase(9, 1), 1.0, 0, 1e-12);
asserts.isapprox(run_switchcase(9, 0), 0.0, 0, 1e-12);
asserts.isapprox(run_switchcase(9, 2), 0.0, 0, 1e-12);
% ctrl = 1 matches case 0.
asserts.isapprox(run_switchcase(1, 0), 1.0, 0, 1e-12);
asserts.isapprox(run_switchcase(1, 1), 0.0, 0, 1e-12);
% ctrl = 5 matches nothing -> default (port 2).
asserts.isapprox(run_switchcase(5, 2), 1.0, 0, 1e-12);
asserts.isapprox(run_switchcase(5, 0), 0.0, 0, 1e-12);
evalin('base', 'clear(''sc_out'')');
%=============================================================================
