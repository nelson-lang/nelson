%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% SSP import (SS2): build two FMI 3.0 FMUs, compose them in an SSP (A.y -> B.u),
% import the SSP into a runnable .nflow, and simulate it. A = constant(5); B =
% gain(2); so B's output is 10. Exercises component -> FMU block mapping and
% connector -> port mapping by name. Skips without the FMI runtime / a toolchain.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);

% FMU A: constant(5) -> external output y.
dA = struct();
dA.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'oy', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
dA.connections = {struct('from', 'k', 'to', 'oy', 'fromIndex', 0, 'toIndex', 0)};
dA.sampleTime = 0.1; dA.duration = 0.5; dA.variables = struct();
fA = [work, filesep, 'A.nflow']; fid = fopen(fA, 'wt'); fwrite(fid, jsonencode(dA)); fclose(fid);
fmuA = NFlow.exportfmu(fA, work);

% FMU B: external input u -> gain(2) -> external output z.
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
fmuB = NFlow.exportfmu(fB, work);

% The FMUs must be loadable binaries (skip if the toolchain produced source-only).
coA = [work, filesep, 'A_check'];
namesA = unzip(fmuA, coA);
hasBinaryA = any(cellfun(@(s) endsWith(s, getdynlibext()), namesA));
skip_testsuite(~hasBinaryA, 'no C toolchain: FMUs source-only, cannot compose an SSP');

% Stage an SSP: SystemStructure.ssd + resources/{A,B}.fmu, connecting A.y -> B.u.
stg = [work, filesep, 'stage']; mkdir(stg); mkdir([stg, filesep, 'resources']);
copyfile(fmuA, [stg, filesep, 'resources', filesep, 'A.fmu']);
copyfile(fmuB, [stg, filesep, 'resources', filesep, 'B.fmu']);
ssd = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
  '<ssd:SystemStructureDescription version="1.0" name="rt" xmlns:ssd="http://ssp-standard.org/SSP1/SystemStructureDescription" xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
  '  <ssd:System name="rt"><ssd:Elements>', char(10), ...
  '    <ssd:Component name="A" source="resources/A.fmu" type="application/x-fmu-sharedlibrary" implementation="CoSimulation"/>', char(10), ...
  '    <ssd:Component name="B" source="resources/B.fmu" type="application/x-fmu-sharedlibrary" implementation="CoSimulation"/>', char(10), ...
  '  </ssd:Elements><ssd:Connections>', char(10), ...
  '    <ssd:Connection startElement="A" startConnector="y" endElement="B" endConnector="u"/>', char(10), ...
  '  </ssd:Connections></ssd:System></ssd:SystemStructureDescription>', char(10)];
fid = fopen([stg, filesep, 'SystemStructure.ssd'], 'wt'); fwrite(fid, ssd); fclose(fid);
sspFile = [work, filesep, 'rt.ssp'];
zip(sspFile, {[stg, filesep, 'SystemStructure.ssd'], [stg, filesep, 'resources']}, [stg, filesep]);

% Import: two FMU blocks and one connection.
nflowPath = NFlow.sspImport(sspFile, work);
assert_istrue(isfile(nflowPath));
d = jsondecode(fileread(nflowPath));
assert_isequal(numel(d.blocks), 2);
assert_isequal(numel(d.connections), 1);

% Simulate with a To Workspace probe on B's output: z = gain(2) * 5 = 10.
di = jsondecode(fileread(nflowPath));
if ~iscell(di.blocks)
  di.blocks = num2cell(di.blocks);
end
if ~iscell(di.connections)
  di.connections = num2cell(di.connections);
end
di.blocks{end + 1} = struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
  'params', struct('VariableName', 'ssp_z', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'));
di.connections{end + 1} = struct('from', 'B', 'fromIndex', 0, 'to', 'tw', 'toIndex', 0);
di.duration = 0.5;
r = jsondecode(__nflow_simulate__(jsonencode(di)));
assert_isequal(r.status, 'done');
z = evalin('base', 'ssp_z'); z = z(:);
% After the first step the composed value settles at 10.
assert_istrue(abs(z(end) - 10.0) < 1e-9);
evalin('base', 'clear(''ssp_z'')');
%=============================================================================
