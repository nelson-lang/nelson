%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(factorial(single(34)), single(2.95232823e+38));
asserts.checkerror('factorial(''a'')', _('Input must be numeric.'));
asserts.checkerror('factorial(true)', _('Input must be numeric.'));
asserts.isequal(factorial(single(33)), single(8.68331851e+36));
asserts.isequal(factorial(single([0 1 2 10 20 30])), single([1 1 2 3628800 2.43290202e+18 2.6525289e+32]));
asserts.isequal(factorial(sparse([1 2 3])), sparse([1 2 6]));
asserts.isequal(factorial(single(25)), single(1.55112111e+25));
asserts.isequal(factorial(single(15)), single(1.30767428e+12));
asserts.isequal(factorial(sparse(5)), sparse(120));
asserts.isequal(factorial(sparse([0 1])), sparse([1 1]));
asserts.isequal(factorial(sparse(zeros(2,2))), sparse(reshape([1 1 1 1], [2 2])));
%=============================================================================
