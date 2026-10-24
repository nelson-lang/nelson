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
skip_testsuite(~__nflow_codegen_toolchain__('C'), 'C toolchain (cmake) not available');
haveRust = __nflow_codegen_toolchain__('rust');
%=============================================================================
% Family 7b -- acausal Newton island: sine source over a series resistor
% and a Shockley diode to ground, sensed across the diode. There is NO
% capacitor, so the island is purely algebraic: every report step is a
% Newton solve of the node voltages with no continuous state to integrate
% (hence no stiffness, and the generated program reads the drive at the
% exact sample instants -- so the two Newton solves must agree to solver
% tolerance). The sine drive sweeps the diode from full reverse blocking
% (sensor ~ -1 V) through the knee to forward conduction each period.
d = struct();
d.blocks = {...
  struct('id', 'V1', 'type', 'SineVoltage', 'params', struct('Amplitude', 1, 'Frequency', 1, 'Phase', 0)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 100)), ...
  struct('id', 'D1', 'type', 'Diode', 'params', struct('Is', 1e-9, 'Vt', 0.025)), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
  struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfdi_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'VS', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.physicalWires = {...
  struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'D1.p', 'VS.p'}}), ...
  struct('pins', {{'V1.n', 'D1.n', 'VS.n', 'G.p'}})};
d.sampleTime = 0.01;
d.duration = 1.0;
d.variables = struct();
d.solver = 'ode4'; % irrelevant (no continuous state) but carries the solver key
dj = NFlow.internal.acausalExpand(jsonencode(d));
nflow_codegen_check_numeric(dj, 'nffam_diode', 'C', {'y'}, {'nfdi_y'}, 1e-6, 1e-7, struct('solver', 'dae'));
if haveRust
  nflow_codegen_check_numeric(dj, 'nffam_diode', 'rust', {'y'}, {'nfdi_y'}, 1e-6, 1e-7, struct('solver', 'dae'));
end
%=============================================================================
