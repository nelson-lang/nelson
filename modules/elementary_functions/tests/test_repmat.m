%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('repmat'), -1)
assert_isequal(nargout('repmat'), 1)
%=============================================================================
R = repmat(1:3, 2, 2);
REF = [1, 2, 3, 1, 2, 3;1, 2, 3, 1, 2, 3];
assert_isequal(R, REF);
%=============================================================================
R = repmat(1:3, 2);
REF = [1, 2, 3, 1, 2, 3;1, 2, 3, 1, 2, 3];
assert_isequal(R, REF);
%=============================================================================
R = repmat(1+20i, 2);
REF = [1+20*i, 1+20*i; 1+20*i, 1+20*i];
assert_isequal(R, REF);
%=============================================================================
R = repmat('Nelson', 3, 2);
REF = ['NelsonNelson';
'NelsonNelson';
'NelsonNelson']
assert_isequal(R, REF);
%=============================================================================
R = repmat(eps, 3, 2);
REF = eps * ones(3, 2);
assert_isequal(R, REF);
%=============================================================================
X = [10 20; 30 40];
R = repmat(X, [2 3 2]);
%=============================================================================
X = [10 20; 30 40];
R = repmat(X, [2 -3 2]);
REF = zeros(4, 0, 2);
assert_isequal(R, REF);
%=============================================================================
