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
asserts.isequal(nargin('regstats'), 4);
asserts.isequal(nargout('regstats'), 1);
%=============================================================================
X = [1 5; 2 4; 3 6; 4 8; 5 7; 6 9];
y = [3.2; 4.1; 5.9; 7.8; 8.4; 10.2];
stats = regstats(y, X, 'linear', {'beta', 'yhat', 'r', 'mse', ...
  'rsquare', 'adjrsquare', 'leverage', 'standres', 'studres', ...
  'cookd', 'tstat', 'fstat', 'dwstat'});
asserts.isequal(stats.source, 'regstats');
asserts.isapprox(stats.beta, [0.406818181818184; 1.106818181818182; ...
                             0.356818181818181], 1e-12);
asserts.isapprox(stats.yhat, [3.297727272727272; 4.047727272727273; ...
                             5.868181818181817; 7.688636363636363; ...
                             8.438636363636364; 10.259090909090908], 1e-12);
asserts.isapprox(stats.r, [-0.097727272727272; 0.052272727272727; ...
                          0.031818181818183; 0.111363636363637; ...
                          -0.038636363636364; -0.059090909090909], 1e-12);
asserts.isapprox(stats.mse, 0.010227272727273, 1e-12);
asserts.isapprox([stats.rsquare stats.adjrsquare], ...
  [0.999146304446805 0.998577174078009], 1e-12);
asserts.isapprox(stats.leverage, [0.659090909090909; 0.659090909090909; ...
                                 0.181818181818183; 0.477272727272728; ...
                                 0.477272727272727; 0.545454545454550], 1e-12);
asserts.isapprox(stats.standres, [-1.655070771676910; 0.885270412757418; ...
                                 0.347832796499981; 1.523091186792525; ...
                                 -0.528419391336183; -0.866666666666664], 1e-12);
asserts.isapprox(stats.studres, [-4.583815401645083; 0.840963287746189; ...
                                0.289910378296916; 2.611708775356326; ...
                                -0.453051522255677; -0.817302997500508], 1e-12);
asserts.isapprox(stats.cookd, [1.765300411522596; 0.505053497942376; ...
                              0.008962048468222; 0.706028145347631; ...
                              0.084982146607857; 0.300444444444448], 1e-12);
asserts.isapprox(stats.tstat.beta, stats.beta, 1e-12);
asserts.isapprox(stats.tstat.se, [0.200528743225265; 0.052074723806316; ...
                                 0.052074723806316], 1e-12);
asserts.isapprox(stats.tstat.t, [2.028727529405513; 21.254422508985691; ...
                                6.852041753410153], 1e-12);
asserts.isapprox(stats.tstat.pval, [0.135510135408937; 0.000227862423876; ...
                                   0.006363176736190], 1e-12);
asserts.isapprox([stats.fstat.f stats.fstat.pval stats.fstat.dfe stats.fstat.dfr], ...
  [1755.566666666666606 0.000024943335536 3 2], 1e-12);
asserts.isapprox(stats.dwstat.dw, 1.700168350168342, 1e-12);
asserts.istrue(isnan(stats.dwstat.pval));
%=============================================================================
selected = regstats(y, X, 'interactions', {'yhat', 'r', 'beta'});
asserts.isequal(fieldnames(selected), {'source'; 'yhat'; 'r'; 'beta'});
asserts.isapprox(selected.beta, [0.053883495145628; 1.218284789644015; ...
                                0.417313915857605; -0.016990291262136], 1e-12);
asserts.isapprox(selected.r, [-0.073786407766988; 0.076213592233010; ...
                             -0.006796116504853; 0.078155339805828; ...
                             -0.071844660194174; -0.001941747572815], 1e-12);
%=============================================================================
Xn = X;
Xn(2, 1) = NaN;
yn = y;
yn(5) = NaN;
missing = regstats(yn, Xn, 'linear', 'beta');
asserts.isapprox(missing.beta, [0.125000000000002; 1.066666666666666; ...
                               0.416666666666667], 1e-12);
%=============================================================================
Dspec = [1 0; 0 1; 1 1; 2 0];
matrixModel = regstats(y, X, Dspec, {'beta', 'R'});
asserts.isapprox(matrixModel.beta, [0.498113207547169; 1.222955974842770; ...
                                   0.274842767295596; 0.030188679245283; ...
                                   -0.048113207547170], 1e-12);
asserts.isequal(size(matrixModel.R), [5 5]);
asserts.isapprox(diag(matrixModel.R), [2.449489742783178; 4.183300132670378; ...
                                      1.942016624910449; 5.300085762599421; ...
                                      3.043375744519579], 1e-12);
%=============================================================================
allStats = regstats(y, X);
expectedFields = {'source'; 'Q'; 'R'; 'beta'; 'covb'; 'yhat'; 'r'; 'mse'; ...
  'rsquare'; 'adjrsquare'; 'leverage'; 'hatmat'; 's2_i'; 'beta_i'; ...
  'standres'; 'studres'; 'dfbetas'; 'dffit'; 'dffits'; 'covratio'; ...
  'cookd'; 'tstat'; 'fstat'; 'dwstat'};
asserts.isequal(fieldnames(allStats), expectedFields);
asserts.isequal(size(allStats.hatmat), [6 6]);
asserts.isequal(size(allStats.beta_i), [3 6]);
asserts.isequal(size(allStats.dfbetas), [3 6]);
%=============================================================================
emptyStats = regstats(zeros(0, 1), zeros(0, 2), 'linear', {'beta', 'mse', 'yhat'});
asserts.isequal(size(emptyStats.beta), [3 1]);
asserts.istrue(all(isnan(emptyStats.beta)));
asserts.istrue(isnan(emptyStats.mse));
asserts.isequal(size(emptyStats.yhat), [0 1]);
%=============================================================================
asserts.checkerror('regstats()', _('Not enough input arguments.'));
asserts.checkerror('regstats({1}, [1])', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('regstats([1], {1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('regstats([1; 2], [1 2])', sprintf(_('%s must have the same number of rows.'), 'Y and X'));
asserts.checkerror('regstats([1; 2], [1; 2], ''bad'', ''beta'')', _('Invalid model specification.'));
asserts.checkerror('regstats([1; 2], [1; 2], ''linear'', ''bad'')', _('Invalid statistic name.'));
asserts.checkerror('regstats([1; 2], [1; 2], [1 0], ''beta'')', sprintf(_('Model matrix must have one column per %s.'), 'predictor'));
%=============================================================================
