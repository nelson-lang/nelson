%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cov'), -1);
assert_isequal(nargout('cov'), 1);
%=============================================================================
R = cov([]);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = cov([1:5]);
REF = 2.5;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = cov([5 0 3 7; 1 -5 7 3; 4 9 8 10]);
REF = [    4.3333    8.8333   -3.0000    5.6667;
8.8333   50.3333    6.5000   24.1667;
-3.0000    6.5000    7.0000    1.0000;
5.6667   24.1667    1.0000   12.3333];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = cov([3 6 4], [7 12 -9]);
REF = [    2.3333    6.8333;
6.8333  120.3333];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = cov([2 0 -9; 3 4 1], [5 2 6; -4 4 9]);
REF = [   22.1667   -6.9333;
-6.9333   19.4667];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
