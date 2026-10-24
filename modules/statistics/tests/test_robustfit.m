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
asserts.isequal(nargin('robustfit'), 5);
asserts.isequal(nargout('robustfit'), 2);
%=============================================================================
x = (1:10)';
y = 10 - 2 * x + [0.2; -0.4; 0.1; 0.3; -0.2; 0.1; 0.4; -0.3; 0.2; -8];
[b, stats] = robustfit(x, y);
asserts.isapprox(b, [9.996240584036739; -1.989201803976405], 1e-12);
asserts.isapprox(stats.resid', [0.192961219939665 -0.417836976083930 0.071364827892476 ...
  0.260566631868882 -0.250231564154712 0.038970239821693 ...
  0.328172043798098 -0.382626152225497 0.106575651750908 ...
  -8.104222544272686], 1e-12);
asserts.isapprox(stats.w', [0.972003256861134 0.888170894672360 0.996939461876441 ...
  0.961811030855536 0.965700795255926 0.999160708462587 ...
  0.939768743308146 0.913904452282784 0.992521593356667 0], 1e-12);
asserts.isapprox([stats.ols_s stats.robust_s stats.mad_s stats.s stats.dfe], ...
  [2.334208710254999 0.266904042243977 0.428764720498747 ...
   0.428764720498747 8], 1e-12);
asserts.isapprox(stats.se', [0.292902065408739 0.047205442103365], 1e-12);
asserts.isapprox(stats.t', [34.128269358862916 -42.139247411785114], 1e-12);
asserts.isequal(size(stats.covb), [2 2]);
asserts.isequal(size(stats.coeffcorr), [2 2]);
asserts.isequal(stats.dfe, 8);
%=============================================================================
[b2, stats2] = robustfit(x, y, 'huber', 1.345, 'off');
asserts.isapprox(b2, -0.637182045062790, 1e-12);
asserts.isapprox(stats2.s, 5.421860470486323, 1e-12);
%=============================================================================
[b3, stats3] = robustfit([x x .^ 2], y, 'ols');
asserts.isapprox(b3, [7.548333333333332; -0.401136363636363; ...
                     -0.184469696969697], 1e-12);
asserts.isequal(stats3.w', ones(1, 10));
%=============================================================================
yN = y;
yN(2) = NaN;
[bn, sn] = robustfit(x, yN);
asserts.isapprox(bn, [10.185369614375634; -2.015499845305263], 1e-12);
asserts.istrue(isnan(sn.resid(2)));
asserts.istrue(isnan(sn.w(2)));
asserts.isapprox(sn.resid([1 3 4 5 6 7 8 9 10])', ...
  [0.030130230929627 -0.038870078459846 0.176629766845417 ...
   -0.307870387849321 0.007629457455943 0.323129302761206 ...
   -0.361370851933532 0.154128993371732 -8.030371161323007], 1e-12);
%=============================================================================
f = @(r) 1 ./ (1 + abs(r));
[bf, sf] = robustfit(x, y, f, 1);
asserts.isapprox(bf, [10.283264148062456; -2.066430720030560], 1e-12);
asserts.isapprox(sf.w([1 2 10])', [0.966008394231784 0.482290123740847 ...
                                  0.059095752794047], 1e-12);
%=============================================================================
asserts.checkerror('robustfit()', _('Not enough input arguments.'));
asserts.checkerror('robustfit({1}, [1])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('robustfit([1], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('robustfit([1; 2], [1])', sprintf(_('%s must have the same number of rows.'), 'X and Y'));
asserts.checkerror('robustfit([1; 2], [1; 2], ''bad'')', sprintf(_('%s has an unsupported value.'), 'WFUN'));
asserts.checkerror('robustfit([1; 2], [1; 2], ''huber'', 0)', sprintf(_('%s must be a positive scalar.'), 'TUNE'));
asserts.checkerror('robustfit([1; 2], [1; 2], ''huber'', [], ''bad'')', sprintf(_('%s has an unsupported value.'), 'CONST'));
%=============================================================================
