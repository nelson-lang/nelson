%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% Regression: a stateSpace block whose A/B/C/D are given as NESTED 2-D matrix
% literals ([[0,1],[-1,-0.4]], ...) must simulate the same system as the flat
% row-major form. A nested matrix used to collapse to one 0.0 per row (the
% descriptor's parseList only resolved the outer array, and a non-scalar json
% resolves to 0.0), silently simulating a wrong, divergent system.
%
% Reference plant: companion (controllable canonical) realisation of the
% oscillator 1/(s^2 + 0.4 s + 1) under a unit step.
%=============================================================================
mk = @(blk)['{"version":"1.0.0","format":"nflow-2","name":"M","blocks":[', ...
  '{"id":"u","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', blk, ',', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"u","to":"blk","fromIndex":0,"toIndex":0},', ...
  '{"from":"blk","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"solver":"ode4","runtime":40,"sampleTime":0.01}'];
%=============================================================================
% Nested 2-D form (rows-of-rows), the format the golden layer already emits.
ssNested = ['{"id":"blk","type":"stateSpace","inputs":1,"outputs":1,', ...
  '"params":{"A":[[0,1],[-1,-0.4]],"B":[[0],[1]],"C":[[1,0]],"D":0}}'];
% Flat row-major form, the format the engine consumes internally.
ssFlat = ['{"id":"blk","type":"stateSpace","inputs":1,"outputs":1,', ...
  '"params":{"A":[0,1,-1,-0.4],"B":[0,1],"C":[1,0],"D":0}}'];
%=============================================================================
rNested = jsondecode(__nflow_simulate__(mk(ssNested)));
rFlat = jsondecode(__nflow_simulate__(mk(ssFlat)));
vNested = rNested.scopes(1).series(:);
vFlat = rFlat.scopes(1).series(:);
n = min(numel(vNested), numel(vFlat));
assert_istrue(n > 100);
% The two forms must be byte-for-byte the same trajectory now.
assert_istrue(max(abs(vNested(1:n) - vFlat(1:n))) < 1e-12);
%=============================================================================
% Physical sanity: a damped oscillator settles to its DC gain (C*(-A)^-1*B = 1)
% and never diverges. The pre-fix nested run peaked well above 2 and grew.
assert_istrue(max(abs(vNested)) < 2.0);
assert_isapprox(vNested(end), 1.0, 1e-2);
%=============================================================================
% Cross-check against the equivalent transfer function 1/(s^2 + 0.4 s + 1):
% the companion realisation above has exactly this transfer function.
tfBlk = ['{"id":"blk","type":"tf","inputs":1,"outputs":1,', ...
  '"params":{"Numerator":[1],"Denominator":[1,0.4,1]}}'];
rTf = jsondecode(__nflow_simulate__(mk(tfBlk)));
vTf = rTf.scopes(1).series(:);
m = min(n, numel(vTf));
assert_istrue(max(abs(vNested(1:m) - vTf(1:m))) < 1e-6);
%=============================================================================
% Discrete dstateSpace is element-wise scalar: a scalar param written in
% bracket form ([0.5]) must resolve to the scalar, not silently collapse to 0
% (paramDouble read resolveNumeric([...]) = 0.0 before the fix, freezing the
% output at 0). A first-order x[k+1] = 0.5 x + u under a unit step settles to 2.
mkd = @(blk)['{"version":"1.0.0","format":"nflow-2","name":"D","blocks":[', ...
  '{"id":"u","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', blk, ',', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"u","to":"blk","fromIndex":0,"toIndex":0},', ...
  '{"from":"blk","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"solver":"discrete","runtime":5,"sampleTime":0.1}'];
dssScalar = ['{"id":"blk","type":"dstateSpace","inputs":1,"outputs":1,', ...
  '"params":{"A":0.5,"B":1,"C":1,"D":0,"Ts":0.1}}'];
dssBracket = ['{"id":"blk","type":"dstateSpace","inputs":1,"outputs":1,', ...
  '"params":{"A":[0.5],"B":[1],"C":[1],"D":[0],"Ts":0.1}}'];
vScalar = jsondecode(__nflow_simulate__(mkd(dssScalar))).scopes(1).series(:);
vBracket = jsondecode(__nflow_simulate__(mkd(dssBracket))).scopes(1).series(:);
nd = min(numel(vScalar), numel(vBracket));
assert_istrue(nd > 10);
assert_istrue(max(abs(vScalar(1:nd) - vBracket(1:nd))) < 1e-12);
assert_isapprox(vBracket(end), 2.0, 1e-2);
%=============================================================================
