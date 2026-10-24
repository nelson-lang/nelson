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
% The discrete state space took scalar A/B/C/D and nothing else: a matrix was
% refused outright, so a discrete plant with more than one state had to be
% written as a continuous one. It now reads the same MIMO form as the
% continuous block - A is nx by nx, B nx by nu, C ny by nx, D ny by nu, all
% row-major - and keeps the scalar form, which applies one first-order system
% independently per signal element.
%=============================================================================
function y = dss_run(params, uValue, stop, dt)
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'dstatespace_mimo';
  d.runtime = stop;
  d.sampleTime = dt;
  d.blocks = {...
    struct('id', 'u', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Value', uValue)), ...
    struct('id', 'f', 'type', 'dstateSpace', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', params), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_dm', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'u', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'f', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(model);
  rmfile(model);
  y = out.y_dm;
  evalin('base', 'clear(''y_dm'')');
end
%=============================================================================
% Two states, one input, one output. The expected answer is the recurrence
% itself, iterated here: x(k+1) = A x(k) + B u, y(k) = C x(k) + D u.
A = [0, 1; -0.5, 1.2];
B = [0; 1];
C = [1, 0];
D = 0;
Ts = 0.1;
u = 1;
p = struct('A', A(:)', 'B', B(:)', 'C', C(:)', 'D', D, 'SampleTime', Ts);
% jsonencode writes the lists row-major, which is how the block reads them.
p.A = [A(1, 1), A(1, 2), A(2, 1), A(2, 2)];
p.B = [B(1), B(2)];
p.C = [C(1), C(2)];
y = dss_run(p, u, 1.0, Ts);
y = y(:)';

x = [0; 0];
expected = zeros(1, numel(y));
for k = 1:numel(y)
  expected(k) = C * x + D * u;
  x = A * x + B * u;
end
assert_istrue(max(abs(y - expected)) < 1e-9);
% and it is not a flat zero, which is what a refused matrix used to leave.
assert_istrue(max(abs(y)) > 0.5);
%=============================================================================
% A non-zero D feeds through on the very first sample.
p2 = p;
p2.D = 3;
y2 = dss_run(p2, u, 0.5, Ts);
assert_isapprox(y2(1), 3, 1e-9, 1e-9);
%=============================================================================
% Two outputs: C has two rows, so the block delivers a width-2 signal.
p3 = p;
p3.C = [1, 0, 0, 1];
y3 = dss_run(p3, u, 0.5, Ts);
assert_isequal(size(y3, 2), 2);
x = [0; 0];
Cm = [1, 0; 0, 1];
for k = 1:size(y3, 1)
  assert_istrue(max(abs(y3(k, :)' - Cm * x)) < 1e-9);
  x = A * x + B * u;
end
%=============================================================================
% The scalar form is untouched: one first-order system per element, so a
% width-2 input carries two independent states.
ps = struct('A', 0.5, 'B', 1, 'C', 2, 'D', 0, 'SampleTime', Ts);
ys = dss_run(ps, [1, 4], 0.5, Ts);
assert_isequal(size(ys, 2), 2);
xs = [0, 0];
for k = 1:size(ys, 1)
  assert_istrue(max(abs(ys(k, :) - 2 * xs)) < 1e-9);
  xs = 0.5 * xs + [1, 4];
end
%=============================================================================
