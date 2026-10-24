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
asserts.isequal(nargin('factoran'), -3);
asserts.isequal(nargout('factoran'), 5);
%=============================================================================
X = [1 2 3; 2 3 5; 4 5 8; 5 7 11; 7 8 13; 8 10 16];
[L, P, T, ST, F] = factoran(X, 2, 'Rotate', 'none');
asserts.isapprox(L, [9.964047340114882e-01 8.468265172366157e-02; ...
                    9.984447881258369e-01 -5.446628935099807e-02; ...
                    9.994455769437112e-01 -3.001325237415515e-02], 1e-6);
asserts.isapprox(P, [6.454536544242551e-06; 1.414283886859158e-04; ...
                    2.077434095776320e-04], 1e-6);
asserts.isequal(T, eye(2));
asserts.isequal(ST.dfe, -2);
asserts.istrue(isnan(ST.chisq));
asserts.istrue(isnan(ST.p));
asserts.isapprox(ST.residual, [0 3.021348545306779e-05 -3.661812978650314e-05; ...
                              3.021348545306779e-05 0 -1.714083302430947e-04; ...
                             -3.661812978650314e-05 -1.714083302430947e-04 0], 1e-6);
asserts.isapprox(F(1:3, :), [-1.271822439911946e+00 -1.283635079358296e-01; ...
                            -9.122778625139123e-01 -4.374330823916539e-02; ...
                            -2.409848158639342e-01 6.789175272819156e-01], 1e-6);
%=============================================================================
[LV, PV, TV, ~, FV] = factoran(X, 2);
asserts.isapprox(LV, [9.289321437816106e-01 3.702412966063570e-01; ...
                     9.712720253209015e-01 2.376746188391767e-01; ...
                     9.651322960506864e-01 2.613654677082505e-01], 1e-6);
asserts.isapprox(PV, P, 1e-6);
asserts.isapprox(TV, [9.569515770925959e-01 2.902476168722032e-01; ...
                    -2.902476168722032e-01 9.569515770925959e-01], 1e-6);
asserts.isapprox(FV(1:3, :), [-1.179815287383794e+00 -4.919810936292732e-01; ...
                             -8.603093483088514e-01 -3.066467033265838e-01; ...
                             -4.276649939427109e-01 5.797459299413544e-01], 1e-6);
%=============================================================================
[LQ, ~, TQ] = factoran(X, 2, 'Rotate', 'quartimax');
asserts.isapprox(LQ, [9.963800692570097e-01 8.497236639550636e-02; ...
                     9.984605827355180e-01 -5.417597562355731e-02; ...
                     9.994542614574355e-01 -2.972264868740324e-02], 1e-6);
asserts.isapprox(TQ, [9.999999577282567e-01 2.907636240965290e-04; ...
                    -2.907636240965290e-04 9.999999577282567e-01], 1e-6);
[LE, ~, TE] = factoran(X, 2, 'Rotate', 'equamax');
asserts.isapprox(LE, [9.289321437816106e-01 3.702412966063570e-01; ...
                     9.712720253209015e-01 2.376746188391767e-01; ...
                     9.651322960506864e-01 2.613654677082505e-01], 1e-6);
asserts.isapprox(TE, [9.569515770925959e-01 2.902476168722032e-01; ...
                    -2.902476168722032e-01 9.569515770925959e-01], 1e-6);
[LO, ~, TO] = factoran(X, 2, 'Rotate', 'orthomax', 'Coeff', 0.5);
asserts.isapprox(LO, [9.963549594277122e-01 8.526629045090113e-02; ...
                     9.984765209666121e-01 -5.388143177129420e-02; ...
                     9.994629860210423e-01 -2.942781276826691e-02], 1e-6);
asserts.isapprox(TO, [9.999998284430561e-01 5.857592153207925e-04; ...
                    -5.857592153207925e-04 9.999998284430561e-01], 1e-6);
%=============================================================================
S = cov(X);
[LC, PC, ~, ~, FC] = factoran(S, 2, 'Xtype', 'covariance', 'Rotate', 'none');
asserts.isapprox(LC, [2.724968319436985e+00 2.727252745379172e-01; ...
                     3.057631323536002e+00 -1.198369513498888e-01; ...
                     4.925345458554395e+00 -7.649220141640102e-02], 1e-6);
asserts.isapprox(PC, [1.685826929893608e-04; 3.196461089308045e-03; ...
                     1.787723686742737e-03], 1e-6);
asserts.istrue(isempty(FC));
%=============================================================================
X4 = [1 2 3 5; 2 3 5 8; 4 5 8 13; 5 7 11 17; ...
      7 8 13 21; 8 10 16 25; 10 13 20 32];
[L4, P4, ~, ST4, F4] = factoran(X4, 1, 'Rotate', 'none');
asserts.isapprox(L4, [9.972157507780260e-01; 9.984809421737756e-01; ...
                     9.996252390029817e-01; 9.999041347005985e-01], 1e-6);
asserts.isapprox(P4, [5.560746400218086e-03; 3.035808115769423e-03; ...
                     7.493815482318977e-04; 1.917214086474273e-04], 1e-6);
asserts.isequal(ST4.dfe, 2);
asserts.isapprox(ST4.chisq, 6.474563458256894e+00, 1e-6);
asserts.isapprox(ST4.p, 3.927049799638815e-02, 1e-6);
asserts.isapprox(F4(1:3), [-1.290141541634173e+00; -9.751512859263118e-01; ...
                          -4.540032843912407e-01], 1e-6);
%=============================================================================
[LR, PR, ~, ~, FR] = factoran(X, 1, 'Scores', 'regression');
asserts.isapprox(LR, [9.964047340114882e-01; 9.984447881258369e-01; ...
                     9.994455769437112e-01], 1e-6);
asserts.isapprox(PR, [7.177606039495243e-03; 3.108005064352604e-03; ...
                     1.108538727652375e-03], 1e-6);
asserts.isapprox(FR(1:3), [-1.277532082310074e+00; -8.942557543458736e-01; ...
                          -2.621633396193256e-01], 1e-6);
[~, ~, ~, ~, FN] = factoran(X, 1, 'Scores', 'none');
asserts.istrue(isempty(FN));
%=============================================================================
asserts.checkerror('factoran()', _('Not enough input arguments.'));
asserts.checkerror('factoran({1}, 1)', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('factoran([1; 2], 1)', _('X must contain at least two variables.'));
asserts.checkerror('factoran([1 2; 3 4], 0)', sprintf(_('%s must be a positive integer scalar.'), 'M'));
asserts.checkerror('factoran([1 2; 3 4], 2)', _('M must be smaller than the number of variables.'));
asserts.checkerror('factoran([1 Inf; 2 3], 1)', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('factoran([1 1; 1 1], 1)', _('Each variable must have positive variance.'));
asserts.checkerror('factoran([1 1; 2 2], 1)', _('X must be positive definite.'));
asserts.checkerror('factoran([1 2; 3 4], 1, ''Rotate'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'Rotate'));
asserts.checkerror('factoran([1 2; 3 4], 1, ''Scores'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'Scores'));
asserts.checkerror('factoran([1 2; 3 4], 1, ''Coeff'', -1)', _('Coeff must be a nonnegative finite scalar.'));
asserts.checkerror('factoran([1 2; 3 4], 1, ''Options'', 1)', sprintf(_('%s must be a scalar structure.'), 'Options'));
asserts.checkerror('factoran([1 2; 3 4], 1, ''Unknown'', 1)', _('Unknown option.'));
asserts.checkerror('factoran([1 2 3], 1, ''Xtype'', ''covariance'')', _('X must be a square matrix when Xtype is covariance.'));
%=============================================================================
