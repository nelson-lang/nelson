%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% loadmat replaces the variables of the calling function, from its very first
% call: its module is loaded on demand, after the call site was prepared.
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'), 'path'), '/mat/'];
mat_file = [mat_dir, 'test_double_7.4_GLNX86.mat'];
testdouble_ref = [0.0000 0.7854 1.5708 2.3562 3.1416 3.9270 4.7124 5.4978 6.2832];
asserts.isapprox(load_into_local(mat_file), testdouble_ref, 1e-4);
asserts.isapprox(load_into_local(mat_file), testdouble_ref, 1e-4);
%=============================================================================
function r = load_into_local(mat_file)
  testdouble = 0;
  loadmat(mat_file);
  r = testdouble;
end
%=============================================================================
