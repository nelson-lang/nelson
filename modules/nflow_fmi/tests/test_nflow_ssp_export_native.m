%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--C CPP COMPILER REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Export a NATIVE nflow diagram (no FMU blocks) as an SSP: NFlow.internal.sspExportContent
% compiles the whole model to one FMI 3.0 FMU component and exposes its external
% ports (labelSource/labelSink) as the SSP's system connectors. A Boolean output
% is cast to Float64 at the FMU boundary automatically, so a logic model exports
% without manual conversion. Round-trip (export -> import -> simulate) must match
% a direct simulation of the same model.
%=============================================================================
work = [tempname(), '_test_ssp_export_native'];
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
%=============================================================================
% (1) Arithmetic model: constant 4 -> gain 2 -> external output 'y'. Exports as
% a 1-component SSP that re-imports and simulates to y = 8.
c = struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'x', 60, 'y', 80, ...
  'params', struct('Value', 4));
g = struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'x', 220, 'y', 80, ...
  'params', struct('Gain', 2));
ysink = struct('id', 'yo', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'x', 380, 'y', 80, ...
  'params', struct('GotoTag', 'y', 'ShowNode', true));
model = struct('blocks', {{c, g, ysink}}, ...
  'connections', {{struct('from', 'c', 'fromIndex', 0, 'to', 'g', 'toIndex', 0), ...
                   struct('from', 'g', 'fromIndex', 0, 'to', 'yo', 'toIndex', 0)}}, ...
  'sampleTime', 0.1, 'duration', 1.0, 'solver', 'discrete', 'variables', struct());
sspPath = [work, filesep, 'Arith.ssp'];
NFlow.internal.sspExportContent(jsonencode(model), sspPath);
assert_istrue(isfile(sspPath));
info = NFlow.sspInfo(sspPath);
assert_isequal(numel(info.components), 1);
out = sim(NFlow.sspImport(sspPath, work));
idx = find(strcmp({out.logsout.name}, 'y'), 1);
assert_istrue(~isempty(idx));
yv = out.logsout(idx).values;
assert_istrue(abs(yv(end) - 8) < 1e-9);
%=============================================================================
% (2) Boolean-output logic model: a compare + AND feeds an external output. The
% FMU boundary is Float64 (automatic coercion), and the SSP round-trip matches a
% direct simulation.
c1 = struct('id', 'c1', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'x', 40, 'y', 60, ...
  'params', struct('Value', 5));
c2 = struct('id', 'c2', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'x', 40, 'y', 160, ...
  'params', struct('Value', 2));
p1 = struct('id', 'p1', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, 'x', 160, 'y', 60, ...
  'params', struct('Operator', '>=', 'Value', 3));
p2 = struct('id', 'p2', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, 'x', 160, 'y', 160, ...
  'params', struct('Operator', '>=', 'Value', 3));
aband = struct('id', 'a', 'type', 'and', 'inputs', 2, 'outputs', 1, 'x', 300, 'y', 110, ...
  'params', struct());
baseConns = {struct('from', 'c1', 'fromIndex', 0, 'to', 'p1', 'toIndex', 0), ...
             struct('from', 'c2', 'fromIndex', 0, 'to', 'p2', 'toIndex', 0), ...
             struct('from', 'p1', 'fromIndex', 0, 'to', 'a', 'toIndex', 0), ...
             struct('from', 'p2', 'fromIndex', 0, 'to', 'a', 'toIndex', 1)};
% Direct simulation (AND -> To Workspace 'q').
tw = struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'x', 440, 'y', 110, ...
  'params', struct('VariableName', 'q'));
directModel = struct('blocks', {{c1, c2, p1, p2, aband, tw}}, ...
  'connections', {[baseConns, {struct('from', 'a', 'fromIndex', 0, 'to', 'tw', 'toIndex', 0)}]}, ...
  'sampleTime', 0.1, 'duration', 0.5, 'solver', 'discrete', 'variables', struct());
dp = [work, filesep, 'direct.nflow'];
fid = fopen(dp, 'wt'); fwrite(fid, jsonencode(directModel)); fclose(fid);
outD = sim(dp);
qd = outD.logsout(1).values;
% SSP round-trip (AND -> labelSink 'q').
qsink = struct('id', 'yo', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'x', 440, 'y', 110, ...
  'params', struct('GotoTag', 'q', 'ShowNode', true));
sspModel = struct('blocks', {{c1, c2, p1, p2, aband, qsink}}, ...
  'connections', {[baseConns, {struct('from', 'a', 'fromIndex', 0, 'to', 'yo', 'toIndex', 0)}]}, ...
  'sampleTime', 0.1, 'duration', 0.5, 'solver', 'discrete', 'variables', struct());
logicSsp = [work, filesep, 'Logic.ssp'];
NFlow.internal.sspExportContent(jsonencode(sspModel), logicSsp);
% The FMU boundary output is Float64 (Boolean coerced).
ex = [work, filesep, 'peek'];
mkdir(ex);
unzip(logicSsp, ex);
fmus = dir([ex, filesep, 'resources', filesep, '*.fmu']);
assert_istrue(~isempty(fmus));
fex = [work, filesep, 'fmu'];
mkdir(fex);
unzip([ex, filesep, 'resources', filesep, fmus(1).name], fex);
md = fileread([fex, filesep, 'modelDescription.xml']);
assert_istrue(~isempty(regexp(md, '<Float64[^>]*name="q"', 'once')));
% Round-trip matches the direct simulation.
outS = sim(NFlow.sspImport(logicSsp, work));
qs = outS.logsout(1).values;
assert_istrue(abs(qs(end) - qd(end)) < 1e-9);
%=============================================================================
