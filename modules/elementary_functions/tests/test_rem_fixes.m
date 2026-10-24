%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(rem(-6,3), -0);
asserts.isequal(rem(-0,3), -0);
asserts.isequal(rem(-6,-3), -0);
asserts.isequal(rem(true,3), 1);
asserts.isequal(rem('a',3), 1);
asserts.isequal(rem(-6.0,2), -0);
asserts.isequal(rem(-0.3,0.1), -0);
asserts.isequal(rem([5 -5 6 -6],[3 3 -3 -3]), [2 -2 0 -0]);
asserts.isequal(rem([0 -0 6 -6],3), [0 -0 0 -0]);
asserts.isequal(rem([0 -0 6 -6],-3), [0 -0 0 -0]);
asserts.isequal(rem(single(-6),single(3)), single(-0));
asserts.isequal(rem(single(-0),single(3)), single(-0));
asserts.isequal(rem([true false],2), [1 0]);
asserts.isequal(rem('ab',2), [1 0]);
asserts.isequal(rem('a','b'), 97);
asserts.isequal(rem(-4,2), -0);
asserts.isequal(rem(-4,-2), -0);
asserts.isequal(rem(-4.5,1.5), -0);
asserts.isequal(rem(-4.5,-1.5), -0);
asserts.isequal(rem(-0.5,0.5), -0);
asserts.isequal(rem(-0.5,-0.5), -0);
asserts.isequal(1 ./ rem(-6,3), -Inf);
asserts.isequal(1 ./ rem(-6,-3), -Inf);
asserts.isequal(1 ./ rem(-0,3), -Inf);
asserts.isequal(1 ./ rem([5 -5 6 -6],[3 3 -3 -3]), [0.5 -0.5 Inf -Inf]);
asserts.isequal(1 ./ rem([0 -0 6 -6],3), [Inf -Inf Inf -Inf]);
asserts.isequal(1 ./ rem(single(-6),single(3)), single(-Inf));
asserts.isequal(1 ./ rem(single([6 -6]),single(3)), single([Inf -Inf]));
%=============================================================================
