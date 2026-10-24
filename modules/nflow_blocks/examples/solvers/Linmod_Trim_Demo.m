%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Linearization / trim demo: a second-order plant analysed with linmod and trim.
%
% The model is a mass-spring-damper written as a state-space block
%     x' = A x + B u,   y = C x + D u,
% with A = [0 1; -w^2 -2*z*w], B = [0; w^2], C = [1 0], D = 0
% (natural frequency w, damping ratio z). Its external input u is an
% external-port Label Source; its output y is a Label Sink.
%
% linmod recovers A, B, C, D numerically; eig(A) gives the poles (stability);
% trim finds the steady state for a constant input, from which the DC gain
% y/u follows. A linear model linearizes to itself, so the recovered matrices
% match the block parameters to finite-difference precision.
%=============================================================================
w = 2.0; % natural frequency
z = 0.5; % damping ratio
A = [0, 1; -w ^ 2, -2 * z * w];
B = [0; w ^ 2];
C = [1, 0];
D = 0;
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'u', 'isExternalPort', true)), ...
  struct('id', 'ss', 'type', 'stateSpace', 'inputs', 1, 'outputs', 1, ...
    'params', struct('A', {num2cell(reshape(A.', 1, []))}, ...
                     'B', {num2cell(B(:)')}, ...
                     'C', {num2cell(C(:)')}, 'D', D)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, ...
    'params', struct('label', 'y'))};
d.connections = {...
  struct('from', 'u', 'to', 'ss', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ss', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01;
d.runtime = 10.0;
d.variables = struct();

model = [tempdir(), 'Linmod_Trim_Demo.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
%=============================================================================
% Linearize: recover the state-space matrices.
[Al, Bl, Cl, Dl] = linmod(model);
disp('Recovered A ='); disp(Al);
disp('Recovered B ='); disp(Bl);
disp('Poles (eig A) ='); disp(eig(Al)); % -1 +/- i*sqrt(3): stable
%=============================================================================
% Trim: steady state for a constant input u0 = 1, then the DC gain y/u.
u0 = 1.0;
[xe, ue, ye] = trim(model, [0; 0], u0);
disp('Equilibrium state xe ='); disp(xe); % [1; 0]
fprintf('Steady-state (DC) gain y/u = %g\n', ye / ue); % 1
%=============================================================================
