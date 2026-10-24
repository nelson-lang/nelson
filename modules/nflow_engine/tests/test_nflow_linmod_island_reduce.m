%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% End-to-end: linearising a LINEAR electrical island exposes the per-
% state DAE mask (differential vs algebraic), so NFlow.internal.reduceDescriptor can
% Schur-eliminate the algebraic node states and recover an explicit ODE state-
% space whose simulation matches the island's own DAE simulation.
%=============================================================================
addpath(modulepath('nflow_blocks', 'functions'));
% A series RLC island: an external voltage-source input (port 0), a voltage
% sensor output (port 0). Fed by an external labelSource for the linearisation.
island = ['{"id":"blk","type":"physicalIsland","inputs":1,"outputs":1,"params":{', ...
  '"domain":"electrical","nodes":4,"components":[', ...
    '{"kind":"vsource","a":1,"b":0,"port":0},', ...
    '{"kind":"resistor","a":1,"b":2,"R":1},', ...
    '{"kind":"inductor","a":2,"b":3,"L":1,"ic":0},', ...
    '{"kind":"capacitor","a":3,"b":0,"C":1,"ic":0},', ...
    '{"kind":"voltageSensor","a":3,"b":0,"outPort":0}]}}'];
linjson = ['{"version":"1.0.0","format":"nflow-2","name":"L","blocks":[', ...
  '{"id":"u","type":"labelSource","inputs":0,"outputs":1,', ...
  '"params":{"GotoTag":"u","isExternalPort":true}},', ...
  strrep(island, '"id":"blk"', '"id":"isl"'), ...
  ',{"id":"y","type":"labelSink","inputs":1,"outputs":1,', ...
  '"params":{"GotoTag":"y","isExternalPort":true}}],', ...
  '"connections":[{"from":"u","to":"isl","fromIndex":0,"toIndex":0},', ...
  '{"from":"isl","to":"y","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":10,"sampleTime":0.05}'];
r = jsondecode(__nflow_linearize__(linjson));
% The lineariser now exposes the per-state DAE mask.
assert_istrue(isfield(r, 'mask'));
mask = r.mask(:).';
assert_isequal(numel(mask), r.nx);
% A series RLC has exactly two differential states (capacitor + inductor); the
% remaining states are algebraic node potentials / branch currents.
assert_isequal(sum(mask == 1), 2);
assert_istrue(sum(mask == 0) >= 1);
%=============================================================================
% Reduce the descriptor to an explicit 2-state ODE.
A = reshape(r.A, r.nx, r.nx).';
B = reshape(r.B, r.nu, r.nx).';
C = reshape(r.C, r.nx, r.ny).';
D = r.D; if (iscell(D) || isempty(D)); D = zeros(r.ny, r.nu); end
[Ar, Br, Cr, Dr] = NFlow.internal.reduceDescriptor(A, B, C, D, mask);
assert_isequal(size(Ar), [2, 2]);
% Same second-order dynamics as the RLC: characteristic poly s^2 + s + 1.
assert_isapprox(trace(Ar), -1, 1e-9);
assert_isapprox(det(Ar), 1, 1e-9);
%=============================================================================
% End-to-end: the reduced state-space (row-major matrices), simulated with a
% fixed-step RK solver, matches the island's own DAE simulation under a step.
mk = @(blk, solver)['{"version":"1.0.0","format":"nflow-2","name":"M","blocks":[', ...
  '{"id":"u","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', blk, ',', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"u","to":"blk","fromIndex":0,"toIndex":0},', ...
  '{"from":"blk","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"solver":"', solver, '","runtime":10,"sampleTime":0.05}'];
ssBlk = sprintf(['{"id":"blk","type":"stateSpace","inputs":1,"outputs":1,', ...
  '"params":{"A":%s,"B":%s,"C":%s,"D":%s}}'], ...
  jsonencode(reshape(Ar.', 1, [])), jsonencode(Br(:).'), jsonencode(Cr(:).'), jsonencode(Dr));
r1 = jsondecode(__nflow_simulate__(mk(island, 'dae')));
r2 = jsondecode(__nflow_simulate__(mk(ssBlk, 'ode4')));
v1 = r1.scopes(1).series(:);
v2 = r2.scopes(1).series(:);
n = min(numel(v1), numel(v2));
% Both settle to ~1 V; the tiny residual is the IDAS-vs-RK4 discretisation only.
assert_istrue(max(abs(v1(1:n) - v2(1:n))) < 1e-4);
assert_isapprox(v2(end), 1.0, 1e-2);
%=============================================================================
