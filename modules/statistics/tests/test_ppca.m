%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('ppca'), -3);
asserts.isequal(nargout('ppca'), 6);
%=============================================================================
Y = [1 2 3; 2 3 5; 4 5 8; 5 7 11; 7 8 13];
[coeff, score, pcvar, mu, v, S] = ppca(Y, 2, ...
  'Options', statset('MaxIter', 20, 'TolFun', 1e-12, 'TolX', 1e-12));
asserts.isapprox(coeff, [0.439583987643785 0.877689815610245; ...
  0.471929899515482 -0.406484914431635; ...
  0.764230389182801 -0.253831837864286], 1e-12);
asserts.isapprox(score(:, 1), [-6.467776809863050; -4.027802144338180; ...
  0.087916797528757; 3.764051751751909; 6.643610404920564], 1e-12);
asserts.isapprox(pcvar, [29.092175285415173; 0.101473255844794], 1e-12);
asserts.isequal(mu, [3.8 5 8]);
asserts.istrue(abs(v - 0.001693722330681) < 1e-12);
asserts.isequal(S.NumIter, 1);
asserts.isapprox(S.Recon(1, :), [0.984149627580871 1.935028193448214 3.049238696319314], 1e-12);
asserts.isapprox(S.RMSResid, 0.041154857923230, 1e-12);
asserts.isequal(size(S.W), [3 2]);
asserts.isequal(size(S.Xexp), [5 2]);
%=============================================================================
Ym = Y;
Ym(2, 2) = NaN;
Ym(4, 3) = NaN;
[cm, sm, pm, mum, vm, Sm] = ppca(Ym, 2, ...
  'Options', statset('MaxIter', 20, 'TolFun', 1e-12, 'TolX', 1e-12));
asserts.isapprox(cm, [0.482024395384306 0.035140713937748; ...
  0.431764325679620 0.859919364209421; ...
  0.762385761491397 -0.509218830446803], 1e-12);
asserts.isapprox(sm(:, 1), [-6.122004707630984; -2.687501744492176; ...
  0.431290263017778; 1.393630955438844; 6.984585233666541], 1e-12);
asserts.isapprox(pm, [23.903564171047122; 1.064779319056134], 1e-12);
asserts.isapprox(mum, [3.800000000000000 5.461336270672396 7.299470222640876], 1e-12);
asserts.istrue(abs(vm - 0.052207242887008) < 1e-12);
asserts.isapprox(Sm.Recon(2, 2), 5.139123262324667, 1e-12);
asserts.isapprox(Sm.Recon(4, 3), 7.718031665263140, 1e-12);
asserts.isapprox(Sm.RMSResid, 0.254560085098954, 1e-12);
%=============================================================================
W0 = [1 0; 0 1; 1 1];
[cw, sw, pw, muw, vw, Sw] = ppca(Ym, 2, 'W0', W0, 'v0', 0.1, ...
  'Options', statset('MaxIter', 3));
asserts.isequal(size(cw), [3 2]);
asserts.isequal(size(sw), [5 2]);
asserts.isequal(size(pw), [2 1]);
asserts.isequal(size(muw), [1 3]);
asserts.istrue(vw > 0);
asserts.isequal(size(Sw.Recon), [5 3]);
%=============================================================================
[cs, ss, ps, mus, vs, Ss] = ppca(single(Y), 2, 'Options', statset('MaxIter', 1));
asserts.isequal(class(cs), 'single');
asserts.isequal(class(ss), 'single');
asserts.isequal(class(ps), 'single');
asserts.isequal(class(mus), 'single');
asserts.isequal(class(vs), 'single');
asserts.isequal(class(Ss.W), 'single');
%=============================================================================
asserts.checkerror('ppca()', _('Not enough input arguments.'));
asserts.checkerror('ppca({1}, 1)', sprintf(_('%s must be a real numeric matrix.'), 'Y'));
asserts.checkerror('ppca([1; 2], 1)', _('Y must contain at least two observations and two variables.'));
asserts.checkerror('ppca([1 Inf; 2 3], 1)', sprintf(_('%s must not contain Inf values.'), 'Y'));
asserts.checkerror('ppca(Y, 0)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('ppca(Y, 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('ppca([NaN 1; NaN 2; NaN 3], 1)', _('Each variable must contain at least one observed value.'));
asserts.checkerror('ppca(Y, 2, ''W0'', ones(2, 2))', sprintf(_('%s has incompatible size.'), 'W0'));
asserts.checkerror('ppca(Y, 2, ''W0'', [1 2; 3 4; 5 Inf])', sprintf(_('%s must contain finite values.'), 'W0'));
asserts.checkerror('ppca(Y, 2, ''v0'', 0)', sprintf(_('%s must be a positive scalar.'), 'v0'));
asserts.checkerror('ppca(Y, 2, ''Options'', 1)', sprintf(_('%s must be a scalar structure.'), 'Options'));
asserts.checkerror('ppca(Y, 2, ''Unknown'', 1)', _('Unknown option.'));
%=============================================================================
