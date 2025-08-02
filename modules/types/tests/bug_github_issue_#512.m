%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/512
% <-- Short Description -->
% Assign must not change left assign type when it is possible
%=============================================================================
A = single(eye(3,3));
A(1,1) = 'f';
assert_isequal(class(A), 'single')
A(2,2) = int32(4);
assert_isequal(class(A), 'single')
A(3,3) = 3;
assert_isequal(class(A), 'single')
REF = single([102, 0, 0; 0 4 0; 0 0 3]);
assert_isequal(A, REF)
A(1,:) = [0 1 0];
assert_isequal(class(A), 'single')
%=============================================================================
