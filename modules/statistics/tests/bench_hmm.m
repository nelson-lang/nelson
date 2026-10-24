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
% <--REUSE PROCESS-->
%=============================================================================
trans = [0.85 0.10 0.05; 0.20 0.65 0.15; 0.10 0.20 0.70];
emis = [0.60 0.30 0.10 0.00; 0.15 0.35 0.35 0.15; 0.05 0.15 0.30 0.50];
rng(0);
[seq, states] = hmmgenerate(2000, trans, emis);
tic();
[te, ee] = hmmestimate(seq, states, 'NStates', 3, 'NSymbols', 4);
[pstates, logp] = hmmdecode(seq, trans, emis);
path = hmmviterbi(seq, trans, emis);
[t2, e2] = hmmtrain(seq, trans, emis, 'MaxIterations', 5);
t = toc();
asserts.isapprox(sum(te, 2), ones(3, 1), 1e-12);
asserts.isapprox(sum(ee, 2), ones(3, 1), 1e-12);
asserts.isequal(size(pstates), [3 2000]);
asserts.isequal(size(path), [1 2000]);
asserts.istrue(isfinite(logp));
asserts.isapprox(sum(t2, 2), ones(3, 1), 1e-12);
asserts.isapprox(sum(e2, 2), ones(3, 1), 1e-12);
asserts.istrue(t < 10);
%=============================================================================
