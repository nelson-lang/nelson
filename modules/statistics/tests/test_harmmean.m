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
asserts.isequal(nargin('harmmean'), -2);
asserts.isequal(nargout('harmmean'), 1);
%=============================================================================
X = reshape(1:30, [3 5 2]);
H1 = harmmean(X);
asserts.isapprox(H1(:, :, 1), [1.636363636363636 4.864864864864865 7.916230366492147 ...
                              10.939226519337018 13.952299829642248], 1e-12);
asserts.isapprox(H1(:, :, 2), [16.960739030023095 19.966638865721436 22.970996216897856 ...
                              25.974346324617660 28.977002379064238], 1e-12);
H2 = harmmean(X, 2);
asserts.isapprox(H2(:, :, 1), [3.185159257962898; 5.064123643538310; 6.569343065693430], 1e-12);
asserts.isapprox(H2(:, :, 2), [21.159506229977652; 22.197943851205544; 23.232944068838357], 1e-12);
H3 = harmmean(X, 3);
expectedH3 = [1.882352941176471 6.608695652173913 10.620689655172413 14.285714285714285 17.756097560975608; ...
              3.578947368421053 8.000000000000000 11.870967741935484 15.459459459459460 18.883720930232560; ...
              5.142857142857143 9.333333333333334 13.090909090909092 16.615384615384617 20.000000000000000];
asserts.isapprox(H3(:), expectedH3(:), 1e-12);
H4 = harmmean(X, [1 2]);
asserts.isequal(size(H4), [1 1 2]);
asserts.isapprox(H4(:), [4.520483676867457; 22.164491514160432], 1e-12);
H5 = harmmean(X, [2 3]);
asserts.isapprox(H5(:), [5.536851364478402; 8.246853054450046; 10.242514564422164], 1e-12);
asserts.isapprox(harmmean(X, 'all'), 7.509410923456069, 1e-12);
%=============================================================================
x = 1:10;
x(3) = NaN;
asserts.istrue(isnan(harmmean(x)));
asserts.isapprox(harmmean(x, 'omitnan'), 3.467359730927992, 1e-12);
asserts.istrue(isnan(harmmean(x, 'includenan')));
asserts.isequal(harmmean([1 2 3], 3), [1 2 3]);
asserts.isequal(harmmean([1 2 3], [3 4]), [1 2 3]);
asserts.isequal(harmmean([1 0 3]), 0);
S = harmmean(single([1 2 4 8]));
asserts.isequal(class(S), 'single');
asserts.isapprox(double(S), 2.133333444595337, 1e-6);
%=============================================================================
asserts.checkerror('harmmean()', _('Not enough input arguments.'));
asserts.checkerror('harmmean({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('harmmean([1 2], 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('harmmean([1 2], ''bad'')', _('Invalid option.'));
asserts.checkerror('harmmean([1 2], ''omitnan'', ''includenan'')', _('Too many input arguments.'));
%=============================================================================
