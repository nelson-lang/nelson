%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% dstateSpace reads scalar A/B/C/D element-wise - one first-order system per
% signal element. Handing it the MIMO matrices the continuous 'stateSpace' block
% accepts used to read back as 0 and emit a flat zero for the whole run, then was
% refused outright; it now reads them as the state space they describe, so the
% two blocks understand a model the same way.
%
% What is checked here is that the matrix form is no longer turned away and no
% longer flat: the recurrence itself is checked in
% test_nflow_dstatespace_mimo.m, and the scalar form below is unchanged.
%=============================================================================
doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'dss_matrix';
doc.runtime = 0.5;
doc.sampleTime = 0.1;
doc.blocks = {...
  struct('id', 'c', 'type', 'constant', 'x', 40, 'y', 160, 'rotation', 0, ...
    'params', struct('Value', 1)), ...
  struct('id', 'f', 'type', 'dstateSpace', 'x', 240, 'y', 160, 'rotation', 0, ...
    'params', struct('A', [0.9, 0.1, 0, 0.8], 'B', [0, 0.1], 'C', [1, 0], ...
    'D', 0, 'SampleTime', 0.1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 160, 'rotation', 0, ...
    'params', struct('VariableName', 'y_dss', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'c', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'f', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model);
y = out.y_dss(:);
rmfile(model);
% A is [0.9 0.1; 0 0.8], B is [0; 0.1], C is [1 0]: driven by a constant 1 the
% first state climbs and the output with it, so it is neither refused nor flat.
asserts.istrue(max(abs(y)) > 1e-3);
asserts.istrue(abs(y(end) - y(1)) > 1e-3);
%=============================================================================
% The scalar form still runs: x(k+1) = 0.5 x(k) + u, y = 2 x, u = 1
% settles at x = 2, y = 4.
doc.blocks{2}.params = struct('A', 0.5, 'B', 1, 'C', 2, 'D', 0, 'SampleTime', 0.1);
doc.runtime = 5;
model2 = [tempname(), '.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model2);
asserts.isapprox(out.y_dss(end), 4, 1e-3, 1e-2);
rmfile(model2);
%=============================================================================
