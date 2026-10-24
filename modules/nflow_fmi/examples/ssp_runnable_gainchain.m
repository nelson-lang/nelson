%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Generate a RUNNABLE SSP composition entirely with nflow, then open and
% simulate it. Unlike ControlledDrivetrain.ssp (whose FMUs are interface-only
% stubs, for a structure-import demo), this SSP carries real FMI 3.0
% Co-Simulation FMUs compiled from nflow models, so File > Open SSP... imports it
% AND Run simulates it.
%
% Composition (two components, one link, one system output):
%
%   [Source: constant 3] --a--> [Gain: x2] --b--> (system output b)
%
% Import wires each component FMU to a block and exposes 'b' as a To Workspace
% output; simulating yields b = 3 * 2 = 6.
%
% Note: the produced .fmu carries a platform-specific binary (e.g. win64), so the
% generated GainChain.ssp only simulates on the platform it was built on. Re-run
% this script on another platform to rebuild native FMUs.
%=============================================================================
work = tempdir();

% --- Component A: a constant source exposing its output as connector 'a' ------
srcConst = struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
  'x', 80, 'y', 80, 'params', struct('Value', 3));
srcOut = struct('id', 'aout', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, ...
  'x', 320, 'y', 80, 'params', struct('GotoTag', 'a', 'ShowNode', true));
sourceModel = struct('blocks', {{srcConst, srcOut}}, ...
  'connections', {{struct('from', 'k', 'fromIndex', 0, 'to', 'aout', 'toIndex', 0)}}, ...
  'sampleTime', 0.1, 'duration', 1.0, 'solver', 'discrete', 'variables', struct());
sourcePath = [work, filesep, 'Source.nflow'];
fid = fopen(sourcePath, 'wt'); fwrite(fid, jsonencode(sourceModel)); fclose(fid);

% --- Component B: gain x2 from connector 'a' to connector 'b' ------------------
gainIn = struct('id', 'ain', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
  'x', 80, 'y', 80, 'params', struct('GotoTag', 'a', 'ShowNode', true));
gainBlk = struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
  'x', 260, 'y', 80, 'params', struct('Gain', 2));
gainOut = struct('id', 'bout', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, ...
  'x', 440, 'y', 80, 'params', struct('GotoTag', 'b', 'ShowNode', true));
gainModel = struct('blocks', {{gainIn, gainBlk, gainOut}}, ...
  'connections', {{struct('from', 'ain', 'fromIndex', 0, 'to', 'g', 'toIndex', 0), ...
                   struct('from', 'g', 'fromIndex', 0, 'to', 'bout', 'toIndex', 0)}}, ...
  'sampleTime', 0.1, 'duration', 1.0, 'solver', 'discrete', 'variables', struct());
gainPath = [work, filesep, 'Gain.nflow'];
fid = fopen(gainPath, 'wt'); fwrite(fid, jsonencode(gainModel)); fclose(fid);

% --- Compose into an SSP: Source.a -> Gain.a, Gain.b -> system output 'b' ------
components = struct('name', {'Source', 'Gain'}, 'model', {sourcePath, gainPath});
connections = struct('startElement', {'Source', 'Gain'}, 'startConnector', {'a', 'b'}, ...
  'endElement', {'Gain', ''}, 'endConnector', {'a', 'b'});
sspPath = [fileparts(mfilename('fullpathext')), '/GainChain.ssp'];
NFlow.sspExport(sspPath, components, connections);
disp(['generated : ', sspPath]);

% --- Prove it: open (import) and simulate -------------------------------------
nflowPath = NFlow.sspImport(sspPath, tempdir());
out = sim(nflowPath);
b = out.logsout(1).values;
fprintf('simulated : %s(end) = %g  (expected 6)\n', out.logsout(1).name, b(end));
%=============================================================================
