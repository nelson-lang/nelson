%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% operator |
%=============================================================================
assert_isequal(sparse(false) | sparse(false), sparse(false));
assert_isequal(sparse(false) | false, false);
assert_isequal(false | sparse(false), false);
assert_isequal(sparse(true) | sparse(false), sparse(true));
assert_isequal(sparse(false) | true, true);
assert_isequal(2 | sparse(false), true);
%=============================================================================
A = sparse(logical(eye(3,3)));
B = 3;
assert_isequal(A | B, logical(ones(3, 3)));
assert_isequal(B | A, logical(ones(3, 3)));
%=============================================================================
A = sparse([true, true]);
B = sparse([true; true]);
R = A | B;
REF = sparse(logical(ones(2, 2)));
assert_isequal(R, REF);
%=============================================================================
% operator &
%=============================================================================
assert_isequal(sparse(false) & sparse(false), sparse(false));
assert_isequal(sparse(true) & sparse(true), sparse(true));
assert_isequal(sparse(false) & false, sparse(false));
assert_isequal(false & sparse(false), sparse(false));
assert_isequal(sparse(true) & sparse(false), sparse(false));
assert_isequal(sparse(false) & true, sparse(false));
assert_isequal(false & sparse(false), sparse(false));
%=============================================================================
R = sparse([false, true]) & sparse(false);
REF = sparse([false, false]);
assert_isequal(R, REF);
%=============================================================================
% operator ||
assert_isequal(sparse(false) || sparse(false), false);
assert_isequal(sparse(false) || false, false);
assert_isequal(false || sparse(false), false);
assert_isequal(sparse(true) || sparse(false), true);
assert_isequal(sparse(false) || true, true);
assert_isequal(2 || sparse(false), true);
%=============================================================================
A = sparse([true, true]);
B = sparse([true; true]);
assert_checkerror('R = A || B;', _('Operand to || operator must be convertible to logical scalar values.'));
%=============================================================================
% operator &&
assert_isequal(true && sparse(true), true);
assert_isequal(sparse(false) && sparse(false), false);
assert_isequal(sparse(false) && false, false);
assert_isequal(false && sparse(false), false);
assert_isequal(sparse(true) && sparse(false), false);
assert_isequal(sparse(true) && sparse(true), true);
assert_isequal(sparse(false) && true, false);
assert_isequal(2 && sparse(false), false);
assert_isequal(2 && sparse(true), true);
%=============================================================================
A = sparse([true, true]);
B = sparse([true; true]);
assert_checkerror('R = A && B;', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================

