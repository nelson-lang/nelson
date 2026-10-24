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
% FMI 2.0 Co-Simulation (F2): an FMI 2.0 FMU runs through the same path as 3.0.
% The fmu block and a direct fmiCoSimulate over the same FMU must agree to 1e-9
% (same FMU, same communication stepping). Dahlquist 2.0: dx/dt = -k*x, k = 1.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
skip_testsuite(nflow_fmi_skip_bundled_fmi2_runtime(), ...
  'bundled FMI 2.0 reference FMUs are not loadable by native macOS arm64 builds');
fmu = [modulepath('nflow_fmi', 'root'), '/examples/Dahlquist_fmi2.fmu'];
%=============================================================================
% Direct co-simulation reference trajectory.
ref = fmiCoSimulate(fmu, 1.0, 0.01);
xref = ref.outputs(:, strcmp(ref.outputNames, 'x'));
assert_istrue(xref(1) > xref(end)); % decays from x0 = 1
assert_istrue(xref(end) > 0); % stays positive
%=============================================================================
% Same FMU as an fmu block, discrete stepper, one To Workspace sink.
d = struct();
d.blocks = {...
  struct('id', 'F', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmu)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'x_blk', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'F', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01;
d.duration = 1.0;
d.solver = 'discrete';
d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
xblk = evalin('base', 'x_blk');
xblk = xblk(:);
%=============================================================================
% Both trajectories must land on the same final value.
assert_istrue(abs(xblk(end) - xref(end)) < 1e-9);
evalin('base', 'clear(''x_blk'')');
%=============================================================================
