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
% Mechanical: a mass-spring-damper authored the GUI way (Mass, Spring/Damper to
% a Fixed wall, PositionSensor) expands to a mechanicalTranslationalIsland and
% matches the analytic underdamped response (m=1,k=4,d=1, s0=1).
docM = struct();
docM.blocks = {...
  struct('id', 'M1', 'type', 'Mass', 'params', struct('m', 1, 's0', 1, 'v0', 0)), ...
  struct('id', 'K1', 'type', 'Spring', 'params', struct('k', 4)), ...
  struct('id', 'D1', 'type', 'Damper', 'params', struct('d', 1)), ...
  struct('id', 'W', 'type', 'Fixed', 'params', struct('s0', 0)), ...
  struct('id', 'PS', 'type', 'PositionSensor', 'params', struct()), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 's'))};
docM.physicalWires = {...
  struct('pins', {{'M1.flange', 'K1.a', 'D1.a', 'PS.flange'}}), ...
  struct('pins', {{'K1.b', 'D1.b', 'W.flange'}})};
docM.connections = {struct('from', 'PS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
docM.solver = 'dae'; docM.runtime = 8.0; docM.sampleTime = 0.05;
exM = NFlow.internal.acausalExpand(jsonencode(docM));
assert_istrue(~isempty(strfind(exM, 'mechanicalTranslationalIsland')));
out4 = sim(nflow_blocks_write_json(exM, 'test_nflow_expand_msd'));
lg4 = out4.logsout; nms4 = {lg4.name};
s = lg4(find(strcmp(nms4, 's'), 1)).values;
t4 = linspace(0, 8, numel(s))';
wn = 2; zeta = 0.25; wd = wn * sqrt(1 - zeta ^ 2);
sref = exp(-zeta * wn * t4) .* (cos(wd * t4) + (zeta * wn / wd) * sin(wd * t4));
assert_istrue(max(abs(s(:) - sref)) < 2e-3);
%=============================================================================
