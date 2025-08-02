%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--JULIA ENVIRONMENT REQUIRED-->
%=============================================================================
currentpath = fileparts(nfilename('fullpathext'));
%=============================================================================
out = evalc('[a, b, c] = jlrunfile([currentpath, ''/test_jlrunfile.jl''], ["A", "B", "C"] ,"D", [10, 20, 30]);');
assert_isequal(a.numeric(), int64([1;2;3]));
assert_isequal(b.numeric(), int64([4;5;6]));
assert_isequal(c.numeric(), [ 15.0  25.0  35.0;17.0  27.0  37.0; 19.0  29.0  39.0]);
out_REF = '"Hello from Julia"
3×3 Matrix{Float64}:
 15.0  25.0  35.0
 17.0  27.0  37.0
 19.0  29.0  39.0
';
assert_isequal(out, out_REF);
%=============================================================================
