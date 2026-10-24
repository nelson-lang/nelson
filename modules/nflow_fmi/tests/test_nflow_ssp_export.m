%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% SSP export (SS4): compose two nflow models into an SSP (A = constant 5, B =
% gain 2, connected A.y -> B.u), then round-trip it back through SSP import and
% simulate. Exercises FMU export per component + SystemStructure.ssd writing +
% connector listing. Skips without the FMI runtime / a C toolchain.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);

% Model A: constant(5) -> external output y.
dA = struct();
dA.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'oy', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
dA.connections = {struct('from', 'k', 'to', 'oy', 'fromIndex', 0, 'toIndex', 0)};
dA.sampleTime = 0.1; dA.duration = 0.5; dA.variables = struct();
fA = [work, filesep, 'A.nflow']; fid = fopen(fA, 'wt'); fwrite(fid, jsonencode(dA)); fclose(fid);

% Model B: external input u -> gain(2) -> external output z.
dB = struct();
dB.blocks = {...
  struct('id', 'iu', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 'oz', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'z', 'isExternalPort', true))};
dB.connections = {...
  struct('from', 'iu', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'oz', 'fromIndex', 0, 'toIndex', 0)};
dB.sampleTime = 0.1; dB.duration = 0.5; dB.variables = struct();
fB = [work, filesep, 'B.nflow']; fid = fopen(fB, 'wt'); fwrite(fid, jsonencode(dB)); fclose(fid);

% A source-only FMU (no toolchain) cannot be inspected/loaded downstream.
probe = NFlow.exportfmu(fA, [work, filesep, 'probe']);
cop = [work, filesep, 'probe_check'];
names = unzip(probe, cop);
hasBinary = any(cellfun(@(s) endsWith(s, getdynlibext()), names));
skip_testsuite(~hasBinary, 'no C toolchain: FMUs source-only');

% Export the composition to an SSP. B.z is routed to a system output 'result',
% so the exported SSP must declare that system connector.
components(1) = struct('name', 'A', 'model', fA);
components(2) = struct('name', 'B', 'model', fB);
connections(1) = struct('startElement', 'A', 'startConnector', 'y', 'endElement', 'B', 'endConnector', 'u');
connections(2) = struct('startElement', 'B', 'startConnector', 'z', 'endElement', '', 'endConnector', 'result');
sspFile = [work, filesep, 'composed.ssp'];
NFlow.sspExport(sspFile, components, connections);
assert_istrue(isfile(sspFile));

% The exported SSP describes the two components and the connections.
info = NFlow.sspInfo(sspFile);
assert_isequal(numel(info.components), 2);
assert_isequal(numel(info.connections), 2);
assert_isequal(info.connections(1).startElement, 'A');
assert_isequal(info.connections(1).endConnector, 'u');
% The system output 'result' is declared as a parent-system connector.
assert_isequal(numel(info.systemConnectors), 1);
assert_isequal(info.systemConnectors(1).name, 'result');
assert_isequal(info.systemConnectors(1).kind, 'output');

% Round-trip: import the exported SSP and simulate; z = gain(2) * 5 = 10.
nflowPath = NFlow.sspImport(sspFile, work);
di = jsondecode(fileread(nflowPath));
if ~iscell(di.blocks)
  di.blocks = num2cell(di.blocks);
end
if ~iscell(di.connections)
  di.connections = num2cell(di.connections);
end
di.blocks{end + 1} = struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
  'params', struct('VariableName', 'exp_z', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'));
di.connections{end + 1} = struct('from', 'B', 'fromIndex', 0, 'to', 'tw', 'toIndex', 0);
di.duration = 0.5;
r = jsondecode(__nflow_simulate__(jsonencode(di)));
assert_isequal(r.status, 'done');
z = evalin('base', 'exp_z'); z = z(:);
assert_istrue(abs(z(end) - 10.0) < 1e-9);
evalin('base', 'clear(''exp_z'')');
%=============================================================================
