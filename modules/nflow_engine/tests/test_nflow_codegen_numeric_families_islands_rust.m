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
% Numeric equivalence of GENERATED code vs the interpreter, part 3 of 3:
% acausal linear island. This family compiles and runs the generated program
% through the local toolchain and compares trajectories sample-by-sample,
% C AND Rust.
% (Split from test_nflow_codegen_numeric_families; see the sibling _basic /
% _lowering files for the other families.)
%=============================================================================
skip_testsuite(~__nflow_codegen_toolchain__('rust'), 'Rust toolchain (cargo) not available');
%=============================================================================
% Family 7a -- acausal linear island: sine-driven RC divider. The palette
% diagram is lowered by NFlow.internal.acausalExpand (the same pass the GUI and the
% codegen hook run) and the LOWERED form feeds both sides. The simulator
% integrates islands through the IDAS backend only (solver 'dae'), while
% the generated program integrates the island under RK4 (ode4): the
% reference runs with the sim override, generation keeps ode4.
d = struct();
d.blocks = {...
  struct('id', 'V1', 'type', 'SineVoltage', 'params', struct('Amplitude', 1, 'Frequency', 1, 'Phase', 0)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 100)), ...
  struct('id', 'C1', 'type', 'Capacitor', 'params', struct('C', 0.001, 'v0', 0)), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
  struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfrc_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'VS', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.physicalWires = {...
  struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'C1.p', 'VS.p'}}), ...
  struct('pins', {{'V1.n', 'C1.n', 'VS.n', 'G.p'}})};
d.sampleTime = 0.01;
d.duration = 1.0;
d.variables = struct();
d.solver = 'ode4';
dj = NFlow.internal.acausalExpand(jsonencode(d));
nflow_codegen_check_numeric(dj, 'nffam_rc_rs', 'rust', {'y'}, {'nfrc_y'}, 2e-4, 1e-4, struct('solver', 'dae'));
%=============================================================================
