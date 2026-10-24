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
% Regression: under a continuous solver the pid's OUTPUT summed only the
% proportional and integral terms - the derivative term was left out of that
% path entirely, so a full PID simply did not derive, while the C/Rust it emits
% carries the term. It was left out because the pid was excluded from the
% discrete update pass (it owns a continuous state), which froze the previous
% input its derivative differences against; the pass now advances that input and
% leaves the integral to the solver.
%=============================================================================
doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'pid_derivative_under_solver';
doc.runtime = 1.0;
doc.sampleTime = 0.01;
doc.solver = 'ode4';
doc.blocks = {...
  struct('id', 'u', 'type', 'step', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Time', 0.5)), ...
  struct('id', 'c', 'type', 'pid', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('P', 1.5, 'I', 2, 'D', 0.1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_pid', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'u', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'c', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model);
t = out.tout(:);
y = out.y_pid(:);
idx = @(tt) find(abs(t - tt) < 1e-9, 1);

% Before the step everything is zero.
asserts.istrue(abs(y(idx(0.49))) < 1e-9);
% At the step the ideal derivative fires one sample of D / dt on top of P.
% The integral is already worth one RK4 weight of the step it has just been
% given: a fixed-step solver crosses a known instant rather than stopping at
% it, so the last stage of the step ENDING at 0.5 sees the input high and
% contributes I * dt / 6.
kRk4 = 2 * 0.01 / 6;
asserts.isapprox(y(idx(0.5)), 1.5 + 0.1 / 0.01 + kRk4, 1e-6, 1e-6);
% One step later the derivative is spent and only P + I remain: the spike must
% not persist (a frozen previous input used to hold it there forever).
asserts.isapprox(y(idx(0.51)), 1.5 + 2 * 0.01 + kRk4, 1e-3, 1e-3);
% The integral keeps accumulating and is NOT advanced twice: at t = 1 the
% integral of a unit step from 0.5 is 0.5, so P + I gives 1.5 + 1.
asserts.isapprox(y(end), 1.5 + 2 * 0.5 + kRk4, 1e-3, 1e-3);
rmfile(model);
%=============================================================================
% A pid with no derivative term is unchanged by any of this.
doc.blocks{2}.params = struct('P', 1.5, 'I', 2, 'D', 0);
model2 = [tempname(), '.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out2 = sim(model2);
asserts.isapprox(out2.y_pid(end), 1.5 + 2 * 0.5 + kRk4, 1e-3, 1e-3);
rmfile(model2);
%=============================================================================
% The fixed-step path (no solver) answers the CURRENT step as well: it used to
% emit what UPDATE had latched, which put the whole block one step behind.
% A ramp of slope 2 through P=2, I=3, D=1 at dt = 0.1: at each sample the output
% is 2u + 3*(Euler integral INCLUDING this step) + 1*2, and the derivative term
% is zero at the first sample (nothing behind it to difference against).
doc3 = struct();
doc3.version = '1.0.0';
doc3.format = 'nflow-2';
doc3.name = 'pid_fixed_step';
doc3.runtime = 0.6;
doc3.sampleTime = 0.1;
doc3.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('slope', 2)), ...
  struct('id', 'c', 'type', 'pid', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('P', 2, 'I', 3, 'D', 1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_fix', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc3.connections = {...
  struct('from', 'r', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'c', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
model3 = [tempname(), '.nflow'];
fid = fopen(model3, 'wt');
fwrite(fid, jsonencode(doc3));
fclose(fid);
out3 = sim(model3);
yf = out3.y_fix(:)';
tf = (0:0.1:0.5);
uf = 2 * tf;
integralEuler = cumsum(uf) * 0.1;
expected = 2 * uf + 3 * integralEuler + [0, 2, 2, 2, 2, 2];
asserts.istrue(numel(yf) >= numel(expected));
asserts.istrue(max(abs(yf(1:numel(expected)) - expected)) < 1e-9);
rmfile(model3);
%=============================================================================
