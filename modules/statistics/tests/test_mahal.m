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
asserts.isequal(nargin('mahal'), 2);
asserts.isequal(nargout('mahal'), 1);
%=============================================================================
X = [1 2; 2 3; 3 5; 4 4; 5 7; 6 8];
Y = [3 4; 5 6; 8 10];
d2 = mahal(Y, X);
asserts.isapprox(d2, [0.180180180180180; 1.261261261261259; 5.805180180180180], 1e-12);
%=============================================================================
X1 = [1; 2; 4; 8];
Y1 = [2; 5; 10];
d2 = mahal(Y1, X1);
asserts.isapprox(d2, [0.319565217391304; 0.163043478260870; 4.076086956521740], 1e-12);
%=============================================================================
d2 = mahal([NaN 3; 3 4], X);
asserts.istrue(isnan(d2(1)));
asserts.isapprox(d2(2), 0.180180180180180, 1e-12);
%=============================================================================
d2 = mahal(zeros(0, 2), X);
asserts.isequal(size(d2), [0 1]);
%=============================================================================
S = mahal(single(Y), single(X));
asserts.isequal(class(S), 'single');
asserts.isapprox(double(S), [0.180180180180180; 1.261261261261259; 5.805180180180180], 1e-6);
%=============================================================================
asserts.checkerror('mahal()', _('Not enough input arguments.'));
asserts.checkerror('mahal([1 2], [1 2])', _('X must have more rows than columns.'));
asserts.checkerror('mahal({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'Y'));
asserts.checkerror('mahal([1; 2], {1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('mahal([1 2], [1; 2; 3])', sprintf(_('X and Y must have %s.'), 'the same number of columns'));
asserts.checkerror('mahal([1 2], [1 2; 3 4])', _('X must have more rows than columns.'));
%=============================================================================
