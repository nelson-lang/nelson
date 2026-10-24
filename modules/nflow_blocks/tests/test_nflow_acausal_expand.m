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
% Acausal palette N0.2: NFlow.internal.acausalExpand lowers a diagram of catalog
% component blocks + physicalWires into native island blocks, driven by
% NFlow.internal.acausalCatalog. Reference: an RC circuit authored the GUI way
% must expand and simulate to 1 - e^{-t}.
%=============================================================================
function p = ex_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
% RC: ConstantVoltage(1) - Resistor(1) - Capacitor(1F, v0=0) - Ground.
% Authored as component blocks with a physicalWires net list and a
% VoltageSensor whose signal output goes to a toWorkspace ("vc").
doc = struct();
doc.blocks = {...
  struct('id', 'V1', 'type', 'ConstantVoltage', 'params', struct('V', 1)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 1)), ...
  struct('id', 'C1', 'type', 'Capacitor', 'params', struct('C', 1, 'v0', 0)), ...
  struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'vc'))};
doc.physicalWires = {...
  struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'C1.p', 'VS.p'}}), ...
  struct('pins', {{'C1.n', 'V1.n', 'VS.n', 'G.p'}})};
doc.connections = {struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
doc.solver = 'dae';
doc.runtime = 1.0;
doc.sampleTime = 0.1;

json = jsonencode(doc);
expanded = NFlow.internal.acausalExpand(json);
% The expansion must have removed physicalWires and introduced an island block.
assert_istrue(isempty(strfind(expanded, 'physicalWires')));
assert_istrue(~isempty(strfind(expanded, 'physicalIsland')));
prepared = NFlow.internal.prepareModelJson(json);
assert_istrue(isempty(strfind(prepared, 'physicalWires')));
assert_istrue(~isempty(strfind(prepared, 'physicalIsland')));

out = sim(ex_write(expanded, 'test_nflow_expand_rc'));
lg = out.logsout; nms = {lg.name};
vc = lg(find(strcmp(nms, 'vc'), 1)).values;
t = linspace(0, 1, numel(vc))';
assert_istrue(max(abs(vc(:) - (1 - exp(-t)))) < 1e-4);

% sim() must auto-expand: pass the ORIGINAL physicalWires document straight to
% sim() (the shared preparation pipeline expands it before the engine).
out2 = sim(ex_write(json, 'test_nflow_expand_rc_sim'));
lg2 = out2.logsout; nms2 = {lg2.name};
vc2 = lg2(find(strcmp(nms2, 'vc'), 1)).values;
t2 = linspace(0, 1, numel(vc2))';
assert_istrue(max(abs(vc2(:) - (1 - exp(-t2)))) < 1e-4);
%=============================================================================
% A diagram with no physicalWires is returned unchanged.
plain = '{"blocks":[{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}}],"connections":[],"solver":"ode4","runtime":1,"sampleTime":0.1}';
assert_istrue(strcmp(NFlow.internal.acausalExpand(plain), plain));
%=============================================================================
bad = doc;
bad.physicalWires{1}.pins = {'V1.missing', 'R1.p'};
assert_checkerror('NFlow.internal.acausalExpand(jsonencode(bad))', ...
  'Physical wire #1 references unknown acausal pin ''V1.missing''.');
%=============================================================================
