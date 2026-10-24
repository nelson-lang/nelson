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
asserts.isequal(nargin('betacdf'), 4);
asserts.isequal(nargout('betacdf'), 1);
%=============================================================================
x = [0 0.1 0.5 0.9 1];
a = [0.5 1 2 5 2];
b = [2 3 2 1 0.5];
R = betacdf(x, a, b);
REF = [0 0.271 0.5 0.59049 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = betacdf(x, a, b, 'upper');
REF = [1 0.729 0.5 0.40951 0];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = betacdf(single(x), single(a), single(b));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single([0 0.271 0.5 0.59049 1]), 1e-6);
%=============================================================================
R = betacdf([-1 2 NaN 0.5 0.5], [2 2 2 -1 2], [2 2 2 2 NaN]);
asserts.isequal(R(1), 0);
asserts.isequal(R(2), 1);
asserts.istrue(all(isnan(R(3:5))));
%=============================================================================
R = betacdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('betacdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('betacdf(1, 2, 1, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'beta'));
asserts.checkerror('betacdf({1}, 2, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('betacdf(1, {2}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('betacdf(1, 2, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
asserts.isapprox(betacdf(0.5, 2, 2), 0.5, 1e-12);
%=============================================================================
