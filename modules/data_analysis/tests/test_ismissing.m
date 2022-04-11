%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ismissing'), 1);
assert_isequal(nargout('ismissing'), 1);
%=============================================================================
A = ["Nel", NaN, "son"];
R = ismissing(A);
REF = [false, true, false];
assert_isequal(R, REF);
%=============================================================================
B = [1 2 NaN Inf];
R = ismissing(B);
REF = [false false true false];
assert_isequal(R, REF);
%=============================================================================
C = 'Nel son';
R = ismissing(C);
REF = [false false false true false false false];
assert_isequal(R, REF);
%=============================================================================
D = {'Nel' '' 'son'};
R = ismissing(D);
REF = [false true false];
assert_isequal(R, REF);
%=============================================================================
