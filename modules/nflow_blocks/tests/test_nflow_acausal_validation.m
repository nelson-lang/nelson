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
% Fail-fast validation of ill-posed acausal diagrams: a non-positive
% capacitance or inductance, and an electrical island with no ground, must be
% rejected up front with a clear, documented error id instead of surfacing as a
% cryptic solver initialisation failure.
%=============================================================================
function c = ac(id, type, params)
  c = struct('id', id, 'type', type, 'x', 0, 'y', 0, 'rotation', 0, 'params', params);
end
%=============================================================================
function sk = mksink(id, name)
  sk = struct('id', id, 'type', 'toWorkspace', 'x', 0, 'y', 0, 'rotation', 0, ...
    'inputs', 1, 'outputs', 0, 'params', struct('VariableName', name, ...
    'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'));
end
%=============================================================================
function m = write_sub_model(inner, ctrlPort)
  % Outer diagram: a single subsystem (plain when ctrlPort is [], controlled when
  % ctrlPort is a controlPort struct) feeding a toWorkspace sink 'vc'.
  subParams = struct('subsystem', inner);
  if ~isempty(ctrlPort)
    subParams.controlPort = ctrlPort;
  end
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'sub';
  d.runtime = 1;
  d.sampleTime = 0.05;
  d.solver = 'dae';
  d.blocks = { ...
    struct('id', 'sub', 'type', 'subsystem', 'inputs', 0, 'outputs', 1, 'x', 200, 'y', 120, ...
      'rotation', 0, 'params', subParams), ...
    mksink('tw', 'vc')};
  d.connections = {struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.physicalWires = {};
  m = [tempname(), '.nflow'];
  fid = fopen(m, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
end
%=============================================================================
function id = sim_error_id(blocks, wires, conns)
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'validation';
  d.runtime = 0.2;
  d.sampleTime = 0.01;
  d.solver = 'dae';
  for k = 1:numel(blocks)
    blocks{k}.x = 60 + 80 * k;
    blocks{k}.y = 120;
  end
  d.blocks = blocks;
  d.connections = conns;
  pw = {};
  for k = 1:numel(wires)
    pw{end + 1} = struct('pins', {wires{k}}); %#ok<AGROW>
  end
  d.physicalWires = pw;
  m = [tempname(), '.nflow'];
  fid = fopen(m, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  id = '';
  try
    sim(m);
  catch e
    id = e.identifier;
  end
  rmfile(m);
end
%=============================================================================
% A zero capacitance is rejected (it would divide by zero in v_C' = i_C/C).
id = sim_error_id({ ...
  ac('V1', 'ConstantVoltage', struct('V', 1)), ac('R1', 'Resistor', struct('R', 1)), ...
  ac('C1', 'Capacitor', struct('C', 0, 'v0', 0)), ac('VS', 'VoltageSensor', struct()), ...
  ac('G', 'Ground', struct()), mksink('tw', 'vc')}, ...
  {{'V1.p', 'R1.p'}, {'R1.n', 'C1.p', 'VS.p'}, {'G.p', 'V1.n', 'C1.n', 'VS.n'}}, ...
  {struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)});
assert_istrue(strcmp(id, 'Nelson:nflow:acausalNonPositiveParam'));
%=============================================================================
% A zero inductance is rejected (it would divide by zero in i_L' = v_L/L).
id = sim_error_id({ ...
  ac('V1', 'ConstantVoltage', struct('V', 2)), ac('R1', 'Resistor', struct('R', 4)), ...
  ac('L1', 'Inductor', struct('L', 0, 'i0', 0)), ac('CS', 'CurrentSensor', struct()), ...
  ac('G', 'Ground', struct()), mksink('tw', 'i')}, ...
  {{'V1.p', 'R1.p'}, {'R1.n', 'L1.p'}, {'L1.n', 'CS.p'}, {'G.p', 'V1.n', 'CS.n'}}, ...
  {struct('from', 'CS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)});
assert_istrue(strcmp(id, 'Nelson:nflow:acausalNonPositiveParam'));
%=============================================================================
% An electrical island with no ground is rejected (no 0-reference node).
id = sim_error_id({ ...
  ac('V1', 'ConstantVoltage', struct('V', 12)), ac('R1', 'Resistor', struct('R', 1)), ...
  ac('R2', 'Resistor', struct('R', 1)), ac('VS', 'VoltageSensor', struct()), mksink('tw', 'vm')}, ...
  {{'V1.p', 'R1.p'}, {'R1.n', 'R2.p', 'VS.p'}, {'R2.n', 'V1.n', 'VS.n'}}, ...
  {struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)});
assert_istrue(strcmp(id, 'Nelson:nflow:acausalNoGround:electricalIsland'));
%=============================================================================
% An acausal island fully contained in a PLAIN (virtual) subsystem is
% lowered to a nested physicalIsland and simulates. Its trace must match the
% closed-form RC step response v(t) = 1 - e^{-t/tau}, tau = R*C = 1*0.5 = 0.5.
inner = struct();
inner.blocks = { ...
  ac('V1', 'ConstantVoltage', struct('V', 1)), ac('R1', 'Resistor', struct('R', 1)), ...
  ac('C1', 'Capacitor', struct('C', 0.5, 'v0', 0)), ac('VS', 'VoltageSensor', struct()), ...
  ac('G', 'Ground', struct()), ...
  struct('id', 'yo', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'x', 0, 'y', 0, ...
    'rotation', 0, 'params', struct('gotoTag', 'vc'))};
inner.connections = {struct('from', 'VS', 'to', 'yo', 'fromIndex', 0, 'toIndex', 0)};
inner.physicalWires = {struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'C1.p', 'VS.p'}}), struct('pins', {{'G.p', 'V1.n', 'C1.n', 'VS.n'}})};
inner.externalOutputs = {struct('id', 'yo', 'name', 'vc')};
msub = write_sub_model(inner, []);
sim(msub);
rmfile(msub);
vc = evalin('base', 'vc');
vc = vc(:);
t = (0:numel(vc) - 1)' * 0.05;
vref = 1 - exp(-t / 0.5);
assert_istrue(numel(vc) == numel(vref));
assert_istrue(max(abs(vc - vref)) < 1e-3);
%=============================================================================
% A CONTROLLED subsystem (here: enable) that contains an acausal component is
% still rejected: a physical (DAE) island cannot be conditionally gated.
mctrl = write_sub_model(inner, struct('kind', 'enable'));
idctrl = '';
try
  sim(mctrl);
catch e
  idctrl = e.identifier;
end
rmfile(mctrl);
assert_istrue(strcmp(idctrl, 'Nelson:nflow:acausalInSubsystemUnsupported'));
%=============================================================================
% A well-formed RC diagram still simulates (no false positive).
id = sim_error_id({ ...
  ac('V1', 'ConstantVoltage', struct('V', 1)), ac('R1', 'Resistor', struct('R', 1)), ...
  ac('C1', 'Capacitor', struct('C', 0.5, 'v0', 0)), ac('VS', 'VoltageSensor', struct()), ...
  ac('G', 'Ground', struct()), mksink('tw', 'vc')}, ...
  {{'V1.p', 'R1.p'}, {'R1.n', 'C1.p', 'VS.p'}, {'G.p', 'V1.n', 'C1.n', 'VS.n'}}, ...
  {struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)});
assert_istrue(isempty(id));
%=============================================================================
