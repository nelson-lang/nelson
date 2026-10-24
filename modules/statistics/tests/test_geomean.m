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
asserts.isequal(nargin('geomean'), -2);
asserts.isequal(nargout('geomean'), 1);
%=============================================================================
X = reshape(1:30, [3 5 2]);
G1 = geomean(X);
asserts.isapprox(G1(:, :, 1), [1.817120592832140 4.932424148660940 7.958114415792782 ...
                              10.969613104865232 13.976149868636853], 1e-12);
asserts.isapprox(G1(:, :, 2), [16.980369497551095 19.983319425122101 22.985498104604215 ...
                              25.987173160227080 28.988501188326641], 1e-12);
G2 = geomean(X, 2);
asserts.isapprox(G2(:, :, 1), [5.154900378820651; 6.578428239440857; 7.815513254092054], 1e-12);
asserts.isapprox(G2(:, :, 2), [21.581362563894338; 22.600373672194777; 23.617700562212111], 1e-12);
G3 = geomean(X, 3);
expectedG3 = [4.000000000000000 8.717797887081348 12.409673645990853 15.811388300841893 19.078784028338912; ...
              5.830951894845301 9.999999999999998 13.564659966250538 16.911534525287760 20.149441679609883; ...
              7.348469228349532 11.224972160321824 14.696938456699069 17.999999999999996 21.213203435596427];
asserts.isapprox(G3(:), expectedG3(:), 1e-12);
G4 = geomean(X, [1 2]);
asserts.isequal(size(G4), [1 1 2]);
asserts.isapprox(G4(:), [6.423424749779763; 22.584511308634223], 1e-12);
G5 = geomean(X, [2 3]);
asserts.isapprox(G5(:), [10.547500844090255; 12.193233221220769; 13.586186064350292], 1e-12);
asserts.isapprox(geomean(reshape(1:40, [2 5 4]), 'all'), 15.768527022963257, 1e-12);
%=============================================================================
x = 1:10;
x(3) = NaN;
asserts.istrue(isnan(geomean(x)));
asserts.isapprox(geomean(x, 'omitnan'), 4.740772189428327, 1e-12);
asserts.istrue(isnan(geomean(x, 'includenan')));
asserts.isequal(geomean([1 2 3], 3), [1 2 3]);
asserts.isequal(geomean([1 2 3], [3 4]), [1 2 3]);
asserts.isequal(geomean([1 0 3]), 0);
S = geomean(single([1 2 4 8]));
asserts.isequal(class(S), 'single');
asserts.isapprox(double(S), 2.828427076339722, 1e-6);
%=============================================================================
asserts.checkerror('geomean()', _('Not enough input arguments.'));
asserts.checkerror('geomean({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('geomean([1 2], 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('geomean([1 2], ''bad'')', _('Invalid option.'));
asserts.checkerror('geomean([1 2], ''omitnan'', ''includenan'')', _('Too many input arguments.'));
%=============================================================================
