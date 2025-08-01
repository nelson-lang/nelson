%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = [double(-3); false];
REF = double([-3;   0]);
assert_isequal(R, REF);
%=============================================================================
R = [false; double(-3)];
REF = double([0; -3]);
assert_isequal(R, REF);
%=============================================================================
A = logical(zeros(3,2,2));
B = double(A);
R = [A; B];
REF = double([B; B]);
assert_isequal(R, REF);
%=============================================================================
