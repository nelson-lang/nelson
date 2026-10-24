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
% Regression: the discrete state space emitted C x(k+1) + D u(k) one step late -
% the state already advanced, and the whole thing held for a step. With a
% non-zero D the block therefore answered a step after its input moved, and the
% emitted C/Rust carried the same shifted formula. It now emits
% y(k) = C x(k) + D u(k) and advances x afterwards.
%=============================================================================
% A = 0.5, B = 1, C = 2, D = 3 with a unit step at t = 0.5, Ts = 0.1:
%   y(0.5) = C*0 + D*1 = 3       (was 0: the D term arrived a step late)
%   x(0.6) = 0.5*0 + 1*1 = 1  -> y(0.6) = 2*1 + 3*1 = 5
%   x(0.7) = 0.5*1 + 1     = 1.5 -> y(0.7) = 2*1.5 + 3 = 6
doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'dstatespace_feedthrough';
doc.runtime = 1.0;
doc.sampleTime = 0.1;
doc.blocks = {...
  struct('id', 'u', 'type', 'step', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Time', 0.5)), ...
  struct('id', 'f', 'type', 'dstateSpace', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('A', 0.5, 'B', 1, 'C', 2, 'D', 3, 'SampleTime', 0.1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_dss', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'u', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'f', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model);
t = out.tout(:);
y = out.y_dss(:);
idx = @(tt) find(abs(t - tt) < 1e-9, 1);

asserts.istrue(abs(y(idx(0.4))) < 1e-9);
asserts.isapprox(y(idx(0.5)), 3, 1e-9, 1e-9);
asserts.isapprox(y(idx(0.6)), 5, 1e-9, 1e-9);
asserts.isapprox(y(idx(0.7)), 6, 1e-9, 1e-9);
rmfile(model);
%=============================================================================
% With D = 0 the block has no feedthrough term and the state readout is
% unchanged: y(0.5) = C x(0.5) = 0, then y(0.6) = 2.
doc.blocks{2}.params = struct('A', 0.5, 'B', 1, 'C', 2, 'D', 0, 'SampleTime', 0.1);
model2 = [tempname(), '.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out2 = sim(model2);
t2 = out2.tout(:);
y2 = out2.y_dss(:);
idx2 = @(tt) find(abs(t2 - tt) < 1e-9, 1);
asserts.istrue(abs(y2(idx2(0.5))) < 1e-9);
asserts.isapprox(y2(idx2(0.6)), 2, 1e-9, 1e-9);
rmfile(model2);
%=============================================================================
