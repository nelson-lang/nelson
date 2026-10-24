%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sim(): run an nflow model from a .nflow file or a loaded system and collect
% the To Workspace results.
%=============================================================================
examples_dir = [modulepath('nflow_blocks'), '/examples'];
%=============================================================================
% From a .nflow file: To Workspace demo is self-contained.
out = sim([examples_dir, '/workspace/To_Workspace_Demo.nflow']);
asserts.istrue(isstruct(out));
asserts.istrue(isfield(out, 'tout'));
asserts.istrue(isfield(out, 'simout'));
asserts.istrue(numel(out.tout) > 1);
%=============================================================================
% StopTime / SampleTime overrides.
out2 = sim([examples_dir, '/workspace/To_Workspace_Demo.nflow'], 'StopTime', 2.0, 'SampleTime', 0.1);
asserts.isapprox(out2.tout(end), 2.0, 0, 1e-10);
%=============================================================================
% From Workspace demo: the input variable must exist in the base workspace.
t = (0:0.01:5)';
assignin('base', 'simin', [t, sin(2 * pi * 0.5 * t)]);
out3 = sim([examples_dir, '/workspace/From_Workspace_Demo.nflow']);
asserts.istrue(isfield(out3, 'simout'));
%=============================================================================
% Scope logs with several input series are returned as one numeric matrix.
outScope = sim([examples_dir, '/causal/complementary_filter.nflow']);
idxScope = find(strcmp({outScope.logsout.name}, 'b11'), 1);
asserts.istrue(~isempty(idxScope));
asserts.isequal(size(outScope.logsout(idxScope).values, 2), 3);
asserts.isequal(size(outScope.logsout(idxScope).values, 1), numel(outScope.logsout(idxScope).time));
%=============================================================================
% A loaded model can be simulated by name.
load_system([examples_dir, '/workspace/To_Workspace_Demo.nflow']);
out4 = sim('To Workspace Demo');
asserts.istrue(isfield(out4, 'simout'));
close_system('To Workspace Demo', 0);
%=============================================================================
% Unknown model: clear error.
msg = '';
try
  sim('no_such_model_zzz');
catch e
  msg = e.identifier;
end
asserts.isequal(msg, 'Nelson:nflow_engine:modelNotFound');
%=============================================================================
% Regression: a continuous state nested inside a subsystem must simulate through
% the sim(file) path. sim() reads the model file, so it decodes and re-encodes
% the diagram; a jsondecode round-trip homogenizes the heterogeneous / nested
% block array and collapses the subsystem's inner diagram (the nested integrator
% then never integrates -> flat zero). sim() now passes the raw model JSON to the
% engine, so an integrator driven by a constant 1 inside a subsystem ramps 0 -> 1.
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, 'params', struct('label', 'in1')), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
nblocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 1, 'outputs', 1, 'params', struct('subsystem', inner)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'yout'))};
nconns = {...
  struct('from', 'c', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
ndoc = struct('blocks', {nblocks}, 'connections', {nconns}, 'runtime', 1, 'sampleTime', 0.1);
nfile = [tempname(), '.nflow'];
fid = fopen(nfile, 'w'); fwrite(fid, jsonencode(ndoc)); fclose(fid);
outn = sim(nfile);
vn = outn.logsout(1).values(:);
asserts.istrue(numel(vn) > 1);
asserts.isapprox(vn(1), 0, 0, 1e-9); % starts at 0
asserts.isapprox(vn(end), 1, 0, 1e-6); % integral of 1 over 1 s -> 1 (was 0 before the fix)
rmfile(nfile);
%=============================================================================
