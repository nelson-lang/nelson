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
asserts.isequal(nargin('unifpdf'), 3);
asserts.isequal(nargout('unifpdf'), 1);
%=============================================================================
x = [0 0.25 0.5 0.75 1];
R = unifpdf(x);
asserts.isequal(R, ones(size(x)));
%=============================================================================
x = 0.2:0.2:1;
R = unifpdf(x);
asserts.isequal(R, [1 1 1 1 1]);
R = unifpdf(1:5, 2, 4);
asserts.isequal(R, [0 0.5 0.5 0.5 0]);
R = unifpdf(5, 10, 1);
asserts.istrue(isnan(R));
%=============================================================================
x = [-2 -1 0 1 2 3];
R = unifpdf(x, -1, 3);
asserts.isequal(R, [0 0.25 0.25 0.25 0.25 0.25]);
%=============================================================================
R = unifpdf([-2 0 3], [-1 -1 1], [1 1 3]);
asserts.isequal(R, [0 0.5 0.5]);
%=============================================================================
R = unifpdf(single([0 0.5 1]), single(0), single(2));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single([0.5 0.5 0.5]));
%=============================================================================
R = unifpdf([0 NaN 1 1], [0 0 2 1], [1 1 1 NaN]);
asserts.isequal(R(1), 1);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = unifpdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('unifpdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('unifpdf(1, 0)', _('Not enough input arguments.'));
asserts.checkerror('unifpdf({1}, 0, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('unifpdf(1, {0}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('unifpdf(1, 0, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
asserts.isequal(unifpdf(0.5, 0, 2), 0.5);
asserts.istrue(isnan(unifpdf(1, 2, 1)));
asserts.checkerror('unifpdf(1, ''a'', 2)', sprintf(_('%s must be real numeric.'), 'A'));
%=============================================================================
