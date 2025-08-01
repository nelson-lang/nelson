%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('rem'), 2)
assert_isequal(nargout('rem'), 1)
%=============================================================================
R = rem(6, uint64(4));
REF = uint64(2);
assert_isequal(R, REF);
assert_isequal(class(R), 'uint64');
%=============================================================================
R = rem(uint64(6), 4);
REF = uint64(2);
assert_isequal(R, REF);
assert_isequal(class(R), 'uint64');
%=============================================================================
R = rem(uint64(6), uint64(4));
REF = uint64(2);
assert_isequal(R, REF);
assert_isequal(class(R), 'uint64');
%=============================================================================
R = rem(single([-6, 6, 0]), [4, 0, 4]);
REF = single([-2 NaN 0]);
assert_isequal(R, REF);
assert_isequal(class(R), 'single');
%=============================================================================
R = rem([], []);
REF = [];
assert_isequal(R, REF);
assert_isequal(class(R), 'double');
%=============================================================================
R = rem(0.94, 0.01);
REF = 0;
assert_isequal(R, REF);
assert_isequal(class(R), 'double');
%=============================================================================
R = rem(66, 44);
REF = 22;
assert_isequal(R, REF);
%=============================================================================
R = rem(NaN, 44);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = rem(Inf, 44);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = rem(-Inf, 44);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = rem(-66, 44);
REF = -22;
assert_isequal(R, REF);
%=============================================================================
R = rem(-6, 4);
REF = -2;
assert_isequal(R, REF);
%=============================================================================
R = rem(0, 4);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
R = rem([-6, 6, 0], [4, 4, 4]);
REF = [-2 2 0];
assert_isequal(R, REF);
%=============================================================================
R = rem([-6; 6; 0], [4; 4; 4]);
REF = [-2; 2; 0];
assert_isequal(R, REF);
%=============================================================================
R = rem([-6, 6; 0, 4], [4, 4 ; 4, 1]);
REF =  [-2 2; 0 0];
assert_isequal(R, REF);
%=============================================================================
R = rem(6, 0);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = rem(-6, 0);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = rem([-6, 6, 0], [4, 0, 4]);
REF = [-2 NaN 0];
assert_isequal(R, REF);
%=============================================================================
R = rem([-6; 6; 0], [4; 0; 4]);
REF = [-2; NaN; 0];
assert_isequal(R, REF);
%=============================================================================
R = rem([-6; 6; 0], single([4; 0; 4]));
REF = single([-2; NaN; 0]);
assert_isequal(R, REF);
assert_isequal(class(R), 'single');
%=============================================================================
R = rem([-6, 6; 0, 4], [4, 0 ; 4, 1]);
REF = [-2 NaN; 0 0];
assert_isequal(R, REF);
assert_isequal(class(R), 'double');
%=============================================================================
R = rem([-6, 6; 0, 4], [0, 0 ; 0, 0]);
REF = [NaN, NaN; NaN, NaN];
assert_isequal(R, REF);
%=============================================================================
R = rem([-6, 6; 0, 4], 0);
REF = [NaN, NaN; NaN, NaN];
assert_isequal(R, REF);
%=============================================================================
R = rem([-6, 6; 0, 4], 4);
REF = [-2 2; 0 0];
assert_isequal(R, REF);
%=============================================================================
R = rem(-6, [0,0; 0,0]);
REF = [NaN, NaN; NaN, NaN];
assert_isequal(R, REF);
%=============================================================================
R = rem(-6, [4,0; 4,1]);
REF = [-2 NaN; -2 0];
assert_isequal(R, REF);
%=============================================================================
R = rem(-6, [4,2; 4,1]);
REF = [-2 0; -2 0];
assert_isequal(R, REF);
%=============================================================================
R = rem(-6, [4,2; 4,1]);
REF = [-2 0; -2 0];
assert_isequal(R, REF);
%=============================================================================
R = rem(uint64 (6), uint64 (4));
REF = uint64 (2);
assert_isequal(R, REF);
%=============================================================================
R = rem(uint64([1:6]), uint64(4));
REF = uint64([1   2   3   0   1   2]);
assert_isequal(R, REF);
%=============================================================================
R = rem(uint64([1:6]), uint64(0));
REF = uint64([ 0   0   0   0   0   0]);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = rem(uint64(6), int64(4));', _('Integers must be combined with integers of the same class.'));
%=============================================================================
R = rem(uint64(6), 4);
REF = uint64(2);
assert_isequal(R, REF);
%=============================================================================
R = rem(2.1, 0.1);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
R = rem(2.1, 0.2);
REF = 0.1;
assert_isapprox(R, REF, 1e-5);
%=============================================================================
