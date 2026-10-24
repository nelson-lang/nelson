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
% The state-space block takes an initial continuous state. Without it a state
% space could only ever start at the origin, and a diagram that starts one away
% from it - an observer seeded off its plant, say - had no way to say so. A
% scalar broadcasts over the states, a list gives them one by one, and an absent
% parameter keeps the historical all zeros.
%=============================================================================
% x' = -x with x(0) = 2, y = x  ->  y(t) = 2 exp(-t).
doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'statespace_initial_condition';
doc.runtime = 1.0;
doc.sampleTime = 0.001;
doc.solver = 'ode4';
doc.blocks = {...
  struct('id', 'u', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 0)), ...
  struct('id', 'ss', 'type', 'stateSpace', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('A', -1, 'B', 0, 'C', 1, 'D', 0, 'InitialCondition', 2)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_ss', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'u', 'to', 'ss', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ss', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model);
y = out.y_ss(:);
asserts.isapprox(y(1), 2, 0, 1e-9);
asserts.isapprox(y(end), 2 * exp(-1), 1e-6, 1e-6);
rmfile(model);
%=============================================================================
% Two states, one initial value each: A = [0 1; 0 0] integrates x2 into x1, so
% x1(t) = 3 + 5 t with x(0) = [3; 5]. C picks x1.
doc.blocks{2}.params = struct('A', [0, 1, 0, 0], 'B', [0, 0], 'C', [1, 0], ...
  'D', 0, 'InitialCondition', [3, 5]);
doc.runtime = 2.0;
model2 = [tempname(), '.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out2 = sim(model2);
y2 = out2.y_ss(:);
asserts.isapprox(y2(1), 3, 0, 1e-9);
asserts.isapprox(y2(end), 3 + 5 * 2, 1e-6, 1e-6);
rmfile(model2);
%=============================================================================
% No InitialCondition: the state still starts at zero.
doc.blocks{2}.params = struct('A', [0, 1, 0, 0], 'B', [0, 0], 'C', [1, 0], 'D', 0);
model3 = [tempname(), '.nflow'];
fid = fopen(model3, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out3 = sim(model3);
asserts.istrue(max(abs(out3.y_ss)) < 1e-9);
rmfile(model3);
%=============================================================================
