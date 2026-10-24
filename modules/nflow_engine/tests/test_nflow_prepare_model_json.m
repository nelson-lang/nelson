%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
doc = struct();
doc.blocks = {...
  struct('id', 'V1', 'type', 'ConstantVoltage', 'params', struct('V', 1)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 1)), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct())};
doc.physicalWires = {...
  struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'V1.n', 'G.p'}})};
doc.connections = {};
doc.solver = 'dae';
doc.runtime = 1;
doc.sampleTime = 0.1;

prepared = NFlow.internal.prepareModelJson(jsonencode(doc));
assert_istrue(isempty(strfind(prepared, 'physicalWires')));
assert_istrue(~isempty(strfind(prepared, 'physicalIsland')));

causal = '{"blocks":[],"connections":[],"solver":"ode4","runtime":1,"sampleTime":0.1}';
assert_istrue(strcmp(NFlow.internal.prepareModelJson(causal), causal));
%=============================================================================
