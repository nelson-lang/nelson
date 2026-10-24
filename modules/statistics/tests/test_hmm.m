%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('hmmgenerate'), 3);
asserts.isequal(nargout('hmmgenerate'), 2);
asserts.isequal(nargin('hmmestimate'), -3);
asserts.isequal(nargout('hmmestimate'), 2);
asserts.isequal(nargin('hmmdecode'), 3);
asserts.isequal(nargout('hmmdecode'), 2);
asserts.isequal(nargin('hmmviterbi'), 3);
asserts.isequal(nargout('hmmviterbi'), 2);
asserts.isequal(nargin('hmmtrain'), -4);
asserts.isequal(nargout('hmmtrain'), 2);
%=============================================================================
trans = [0.7 0.3; 0.4 0.6];
emis = [0.5 0.4 0.1; 0.1 0.3 0.6];
seq = [1 2 3 2 1];
states = [1 1 2 2 1];
%=============================================================================
[TE, EE] = hmmestimate(seq, states, 'NStates', 2, 'NSymbols', 3);
asserts.isapprox(TE, [0.5 0.5; 0.5 0.5], 1e-12);
asserts.isapprox(EE, [0.666666666666667 0.333333333333333 0; ...
                     0 0.5 0.5], 1e-12);
%=============================================================================
[P, L] = hmmdecode(seq, trans, emis);
asserts.isapprox(P, [0.827099888319756 0.615965390225869 0.242081915127455 ...
                    0.626706010084526 0.864378442826598; ...
                    0.172900111680244 0.384034609774131 0.757918084872545 ...
                    0.373293989915474 0.135621557173402], 1e-12);
asserts.isapprox(L, -5.625948481320407, 1e-12);
asserts.isapprox(sum(P, 1), ones(1, length(seq)), 1e-12);
%=============================================================================
[S, LV] = hmmviterbi(seq, trans, emis);
asserts.isequal(S, [1 1 2 1 1]);
asserts.isapprox(LV, -7.256462053271693, 1e-12);
%=============================================================================
[TT, ET] = hmmtrain(seq, trans, emis, 'MaxIterations', 5, 'Tolerance', 1e-12);
asserts.isapprox(TT, [0.401227103276561 0.598772896723439; ...
                     0.365847602710961 0.634152397289039], 1e-12);
asserts.isapprox(ET, [0.606959230332917 0.336099925517745 0.056940844149339; ...
                     0.233664552722462 0.451357204281134 0.314978242996403], 1e-12);
[TT2, ET2] = hmmtrain(seq, trans, emis, 'MaxIterations', 2, ...
  'PseudoTransitions', 0.1, 'PseudoEmissions', 0.1);
asserts.isapprox(TT2, [0.582115001272020 0.417884998727980; ...
                      0.474563431237507 0.525436568762493], 1e-12);
asserts.isapprox(ET2, [0.507906339458507 0.378942194617825 0.113151465923668; ...
                      0.244663933633516 0.410780786799744 0.344555279566739], 1e-12);
%=============================================================================
rng(0);
[G, GS] = hmmgenerate(8, trans, emis);
asserts.isequal(G, [3 3 1 2 3 3 3 2]);
asserts.isequal(GS, [2 1 1 1 2 1 2 2]);
[G0, GS0] = hmmgenerate(0, trans, emis);
asserts.isequal(G0, zeros(1, 0));
asserts.isequal(GS0, zeros(1, 0));
%=============================================================================
asserts.checkerror('hmmgenerate(-1, trans, emis)', sprintf(_('%s must be a nonnegative integer scalar.'), 'LEN'));
asserts.checkerror('hmmdecode([1 4], trans, emis)', sprintf(_('%s contains invalid indices.'), 'SEQ'));
asserts.checkerror('hmmviterbi([1 NaN], trans, emis)', sprintf(_('%s contains invalid indices.'), 'SEQ'));
asserts.checkerror('hmmestimate({1}, states)', sprintf(_('%s must be a nonempty numeric vector.'), 'SEQ'));
asserts.checkerror('hmmestimate(seq, [1 2])', sprintf(_('%s must have the same length.'), 'SEQ and STATES'));
asserts.checkerror('hmmestimate(seq, states, ''NStates'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'NStates'));
asserts.checkerror('hmmtrain(seq, eye(3), emis)', _('TRANS and EMIS must have the same number of states.'));
asserts.checkerror('hmmtrain(seq, trans, emis, ''MaxIterations'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MaxIterations'));
asserts.checkerror('hmmtrain(seq, trans, emis, ''Tolerance'', 0)', sprintf(_('%s must be a positive scalar.'), 'Tolerance'));
asserts.checkerror('hmmtrain(seq, trans, emis, ''PseudoTransitions'', ones(3))', sprintf(_('%s has incompatible size.'), 'PseudoTransitions'));
%=============================================================================
