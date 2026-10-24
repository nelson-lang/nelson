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
asserts.isequal(nargin('nbininv'), 3);
asserts.isequal(nargout('nbininv'), 1);
%=============================================================================
y = [0 0.1 0.5 0.9 1];
R = nbininv(y, 3, 0.4);
asserts.isequal(R(1:4), [0 1 4 9]);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = nbininv([0.25 0.5 0.75], [1 2.5 3], [0.5 0.4 1]);
asserts.isequal(R, [0 3 0]);
R = nbininv([0 0.5], 3, [0 0]);
asserts.isequal(R(1), 0);
asserts.istrue(isinf(R(2)) && R(2) > 0);
%=============================================================================
R = nbininv(single([0.1 0.5]), single(3), single(0.4));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single([1 4]));
%=============================================================================
R = nbininv([-0.1 1.1 NaN 0.5], 3, 0.4);
asserts.istrue(all(isnan(R(1:3))));
asserts.isequal(R(4), 4);
R = nbininv([0.5 0.5 0.5], [0 -1 3], [0.4 0.4 1.1]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = nbininv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('nbininv(ones(2), ones(3), 0.5)', _('Size mismatch on input arguments'));
asserts.checkerror('nbininv({0.5}, 3, 0.4)', sprintf(_('%s must be real numeric.'), 'Y'));
asserts.checkerror('nbininv(0.5, {3}, 0.4)', _('R must be a positive numeric array.'));
asserts.checkerror('nbininv(0.5, 3, {0.4})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
