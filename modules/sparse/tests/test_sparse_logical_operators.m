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
asserts.isequal(sparse(false) | sparse(false), sparse(false));
asserts.isequal(sparse(false) | false, false);
asserts.isequal(false | sparse(false), false);
asserts.isequal(sparse(true) | sparse(false), sparse(true));
asserts.isequal(sparse(false) | true, true);
asserts.isequal(2 | sparse(false), true);
%=============================================================================
A = sparse(logical(eye(3, 3)));
B = 3;
asserts.isequal(A | B, logical(ones(3, 3)));
asserts.isequal(B | A, logical(ones(3, 3)));
%=============================================================================
A = sparse([true, true]);
B = sparse([true; true]);
R = A | B;
REF = sparse(logical(ones(2, 2)));
asserts.isequal(R, REF);
%=============================================================================
% operator &
%=============================================================================
asserts.isequal(sparse(false) & sparse(false), sparse(false));
asserts.isequal(sparse(true) & sparse(true), sparse(true));
asserts.isequal(sparse(false) & false, sparse(false));
asserts.isequal(false & sparse(false), sparse(false));
asserts.isequal(sparse(true) & sparse(false), sparse(false));
asserts.isequal(sparse(false) & true, sparse(false));
asserts.isequal(false & sparse(false), sparse(false));
%=============================================================================
R = sparse([false, true]) & sparse(false);
REF = sparse([false, false]);
asserts.isequal(R, REF);
%=============================================================================
% operator ||
asserts.isequal(sparse(false) || sparse(false), false);
asserts.isequal(sparse(false) || false, false);
asserts.isequal(false || sparse(false), false);
asserts.isequal(sparse(true) || sparse(false), true);
asserts.isequal(sparse(false) || true, true);
asserts.isequal(2 || sparse(false), true);
%=============================================================================
A = sparse([true, true]);
B = sparse([true; true]);
asserts.checkerror('R = A || B;', _('Operand to || operator must be convertible to logical scalar values.'));
%=============================================================================
% operator &&
asserts.isequal(true && sparse(true), true);
asserts.isequal(sparse(false) && sparse(false), false);
asserts.isequal(sparse(false) && false, false);
asserts.isequal(false && sparse(false), false);
asserts.isequal(sparse(true) && sparse(false), false);
asserts.isequal(sparse(true) && sparse(true), true);
asserts.isequal(sparse(false) && true, false);
asserts.isequal(2 && sparse(false), false);
asserts.isequal(2 && sparse(true), true);
%=============================================================================
A = sparse([true, true]);
B = sparse([true; true]);
asserts.checkerror('R = A && B;', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================

