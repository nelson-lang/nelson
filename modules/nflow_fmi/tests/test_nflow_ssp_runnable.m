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
% End-to-end runnable SSP: compose two nflow models into an SSP whose components
% are real FMI 3.0 Co-Simulation FMUs (compiled binaries), import it, and
% simulate. Source(constant 3) -a-> Gain(x2) -b-> system output 'b' => b = 6.
% Unlike the interface-only ControlledDrivetrain.ssp, this exercises actual FMU
% instantiation and stepping. Needs a C toolchain to build the FMU binaries.
% See examples/ssp_runnable_gainchain.m (the committed GainChain.ssp).
%=============================================================================
work = [tempname(), '_test_ssp_runnable'];
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
%=============================================================================
% Component A: constant 3 -> connector 'a'.
ca = struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'x', 80, 'y', 80, ...
  'params', struct('Value', 3));
ao = struct('id', 'aout', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'x', 320, 'y', 80, ...
  'params', struct('GotoTag', 'a', 'ShowNode', true));
srcModel = struct('blocks', {{ca, ao}}, ...
  'connections', {{struct('from', 'k', 'fromIndex', 0, 'to', 'aout', 'toIndex', 0)}}, ...
  'sampleTime', 0.1, 'duration', 1.0, 'solver', 'discrete', 'variables', struct());
sp = [work, filesep, 'Source.nflow'];
fid = fopen(sp, 'wt'); fwrite(fid, jsonencode(srcModel)); fclose(fid);
%=============================================================================
% Component B: connector 'a' -> gain x2 -> connector 'b'.
bi = struct('id', 'ain', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, 'x', 80, 'y', 80, ...
  'params', struct('GotoTag', 'a', 'ShowNode', true));
bg = struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'x', 260, 'y', 80, ...
  'params', struct('Gain', 2));
bo = struct('id', 'bout', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'x', 440, 'y', 80, ...
  'params', struct('GotoTag', 'b', 'ShowNode', true));
gainModel = struct('blocks', {{bi, bg, bo}}, ...
  'connections', {{struct('from', 'ain', 'fromIndex', 0, 'to', 'g', 'toIndex', 0), ...
                   struct('from', 'g', 'fromIndex', 0, 'to', 'bout', 'toIndex', 0)}}, ...
  'sampleTime', 0.1, 'duration', 1.0, 'solver', 'discrete', 'variables', struct());
gp = [work, filesep, 'Gain.nflow'];
fid = fopen(gp, 'wt'); fwrite(fid, jsonencode(gainModel)); fclose(fid);
%=============================================================================
% Compose and export a real SSP (each model -> an FMI 3.0 CS FMU with a binary).
components = struct('name', {'Source', 'Gain'}, 'model', {sp, gp});
connections = struct('startElement', {'Source', 'Gain'}, 'startConnector', {'a', 'b'}, ...
  'endElement', {'Gain', ''}, 'endConnector', {'a', 'b'});
sspPath = [work, filesep, 'GainChain.ssp'];
NFlow.sspExport(sspPath, components, connections);
assert_istrue(isfile(sspPath));
%=============================================================================
% Import back to a runnable diagram: two FMU blocks + one To Workspace sink.
nflowPath = NFlow.sspImport(sspPath, work);
doc = jsondecode(fileread(nflowPath));
types = cell(1, numel(doc.blocks));
for i = 1:numel(doc.blocks)
  types{i} = doc.blocks(i).type;
end
assert_isequal(sum(strcmp(types, 'fmu')), 2);
assert_isequal(sum(strcmp(types, 'toWorkspace')), 1);
%=============================================================================
% Simulate: Source(3) * Gain(2) = 6.
out = sim(nflowPath);
idx = find(strcmp({out.logsout.name}, 'b'), 1);
assert_istrue(~isempty(idx));
vals = out.logsout(idx).values;
assert_istrue(abs(vals(end) - 6) < 1e-9);
%=============================================================================
