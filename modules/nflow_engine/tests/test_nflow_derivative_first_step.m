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
% Regression: the derivative block took the input before the first sample to be
% 0, so its first output was u(0) / dt. The derivative of a constant 5 came back
% as 500 at t = 0, and any diagram whose input does not start at 0 got that
% spike rung through everything downstream. The first sample has nothing behind
% it to difference against, so the derivative there is zero.
%=============================================================================

doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'derivative_first_step';
doc.runtime = 0.05;
doc.sampleTime = 0.01;
doc.blocks = {...
  struct('id', 'c', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 5)), ...
  struct('id', 'd', 'type', 'derivative', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct()), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_der', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'c', 'to', 'd', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'd', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model);
% d/dt of a constant is zero, first sample included.
asserts.istrue(max(abs(out.y_der)) < 1e-9);
rmfile(model);
%=============================================================================
% A ramp of slope 3 starting at a non-zero offset: the derivative is 3 from the
% second sample on, and zero at the first one.
doc.blocks{1} = struct('id', 'c', 'type', 'ramp', 'x', 40, 'y', 100, ...
  'rotation', 0, 'params', struct('slope', 3, 'start', 0));
doc.blocks{end}.params.VariableName = 'y_ramp';
doc.blocks{2}.params = struct();
doc.runtime = 0.2;
% Push the ramp up so the input does not start at zero.
doc.blocks{end + 1} = struct('id', 'b', 'type', 'bias', 'x', 140, 'y', 100, ...
  'rotation', 0, 'params', struct('Bias', 7));
doc.connections = {...
  struct('from', 'c', 'to', 'b', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'd', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'd', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
model2 = [tempname(), '.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out2 = sim(model2);
y = out2.y_ramp(:);
asserts.istrue(abs(y(1)) < 1e-9);
asserts.isapprox(y(end), 3, 1e-6, 1e-6);
rmfile(model2);
%=============================================================================
