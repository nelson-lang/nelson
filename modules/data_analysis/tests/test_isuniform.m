%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_istrue(isuniform([1 2 3]));
assert_isfalse(isuniform([1 2 4]));
assert_isfalse(isuniform(1));
assert_isfalse(isuniform([]));
assert_istrue(isuniform(0:0.1:1));
assert_istrue(isuniform([3 2 1]));
assert_istrue(isuniform([1; 2; 3]));
assert_istrue(isuniform(int8([1 2 3])));
assert_istrue(isuniform([true false]));
assert_isfalse(isuniform([1 NaN 3]));
assert_isfalse(isuniform([1 Inf]));
assert_isfalse(isuniform([0 0.1 0.2 + 1e-15]));
assert_isfalse(isuniform([1e6 1e6 + 1 1e6 + 2 + 1e-9]));
[tf, step] = isuniform([1 3 5]);
assert_istrue(tf);
assert_isequal(step, 2);
[tf, step] = isuniform([1 3 6]);
assert_isfalse(tf);
assert_istrue(isnan(step));
[tf, step] = isuniform(single([1 1.5 2]));
assert_isequal(step, single(0.5));
[tf, step] = isuniform(int8([1 3 5]));
assert_isequal(step, 2);
[tf, step] = isuniform([2 2 2]);
assert_istrue(tf);
assert_isequal(step, 0);
assert_checkerror('isuniform([1 2; 3 4])', _('Input must be a vector.'));
assert_checkerror('isuniform([1i 2i])', _('Input must be numeric and real.'));
%=============================================================================
