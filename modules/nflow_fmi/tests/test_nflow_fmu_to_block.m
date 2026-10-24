%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% FMU import assistant: fmuToBlock turns an FMU into a native nflow
% block manifest -- ports from causalities, parameters, icon, preset path -- and
% the generated block simulates through the FMI path. Uses a shipped reference
% FMU, so it needs no OpenModelica.
%=============================================================================
vdp = [modulepath('nflow_fmi'), '/examples/VanDerPol.fmu'];
%=============================================================================
% Ports are derived from the FMU model description (VanDerPol: 2 outputs, no
% inputs, one parameter mu). The block targets the engine's fmu handler.
block = fmuToBlock(vdp, 'Name', 'VanDerPol');
assert_isequal(block.type, 'fmu');
assert_isequal(block.label, 'VanDerPol');
assert_isequal(numel(block.inputs), 0);
assert_istrue(numel(block.outputs) >= 1);
assert_istrue(numel(block.parameters) >= 1);
% Parameter defaults: a struct name -> FMU start value, one field per
% parameter, mirrored into defaultParams.parameterValues.
assert_istrue(isstruct(block.parameterDefaults));
assert_isequal(numel(fieldnames(block.parameterDefaults)), numel(block.parameters));
assert_istrue(isfield(block.defaultParams, 'parameterValues'));
assert_isequal(block.defaultParams.parameterValues, block.parameterDefaults);
assert_istrue(isfield(block.defaultParams, 'outputWidths'));
assert_isequal(numel(block.defaultParams.outputWidths), numel(block.outputs));
% fmiInfo now surfaces per-variable start values (NaN when none) and descriptions.
fi0 = fmiInfo(vdp);
assert_istrue(isfield(fi0, 'startValues'));
assert_istrue(isfield(fi0, 'descriptions'));
assert_isequal(numel(fi0.startValues), numel(fi0.names));
% Each output port carries the FMU variable name and sits on the right side.
for k = 1:numel(block.outputs)
  assert_istrue(isfield(block.outputs{k}, 'name'));
  assert_isequal(block.outputs{k}.side, 'right');
end
% The preset path points back at the FMU; render is present (icon or fallback).
assert_isequal(block.defaultParams.path, strrep(vdp, '\', '/'));
assert_istrue(isfield(block, 'render'));
%=============================================================================
% Writing a one-block library.json yields valid, reloadable JSON.
libFile = [tempname(), '.json'];
b2 = fmuToBlock(vdp, 'Name', 'VanDerPol', 'LibraryFile', libFile);
assert_istrue(isfile(libFile));
lib = jsondecode(fileread(libFile));
assert_istrue(isfield(lib, 'blocks'));
assert_istrue(~isempty(lib.blocks));
%=============================================================================
% The generated block, placed in a model with its preset path, simulates: the
% Van der Pol oscillator returns a bounded, non-constant trajectory.
m = struct('id', 'g', 'type', block.type, 'inputs', numel(block.inputs), ...
           'outputs', numel(block.outputs), 'params', block.defaultParams);
tw = struct('id', 'o', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
            'params', struct('VariableName', 'y'));
conn = struct('from', 'g', 'to', 'o', 'fromIndex', 0, 'toIndex', 0);
doc = struct('blocks', {{m, tw}}, 'connections', {{conn}}, 'runtime', 5.0, 'sampleTime', 0.05);
p = [tempname(), '.nflow'];
fid = fopen(p, 'w'); fwrite(fid, jsonencode(doc)); fclose(fid);
out = sim(p);
y = out.logsout(1).values;
assert_istrue(numel(y) > 1);
assert_istrue(max(abs(y)) > 0); % non-trivial signal
assert_istrue(all(isfinite(y(:)))); % bounded
%=============================================================================
