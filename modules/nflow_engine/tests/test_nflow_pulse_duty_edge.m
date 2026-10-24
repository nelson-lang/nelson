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
% Regression: the pulse duty boundary was computed as (Width / 100) * Period.
% Width / 100 rounds up (40 / 100 lands just above 0.4), so the boundary came
% out a couple of ulps too late and a sample landing exactly on the falling edge
% read as still inside the pulse - the pulse stayed high for one extra sample at
% every edge. In a fixed-step run edges land on samples all the time, so this
% was visible on any square-ish source and on everything downstream of it.
%=============================================================================
doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'pulse_duty_edge';
doc.runtime = 1.0;
doc.sampleTime = 0.01;
doc.blocks = {...
  struct('id', 'u', 'type', 'pulse', 'x', 40, 'y', 160, 'rotation', 0, ...
    'params', struct('Amplitude', 2, 'Period', 1.5, 'Width', 40, ...
    'StartTime', 0.25, 'Offset', 0)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 240, 'y', 160, 'rotation', 0, ...
    'params', struct('VariableName', 'y_pulse_edge', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'u', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model);
y = out.y_pulse_edge(:);
t = out.tout(:);

% On for [0.25, 0.85), off from 0.85: 0.25 + 40 % of 1.5 = 0.85 exactly, and the
% interval is half-open, so the sample AT 0.85 is already low.
idx = @(tt) find(abs(t - tt) < 1e-9, 1);
asserts.isequal(y(idx(0.24)), 0);
asserts.isequal(y(idx(0.25)), 2);
asserts.isequal(y(idx(0.84)), 2);
asserts.isequal(y(idx(0.85)), 0);
asserts.isequal(y(idx(0.86)), 0);
rmfile(model);
%=============================================================================
