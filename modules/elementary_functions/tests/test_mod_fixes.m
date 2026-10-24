%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(mod(0,-3), -0);
asserts.isequal(mod(6,-3), -0);
asserts.isequal(mod(-0,0), 0);
asserts.isequal(mod(0,-0), -0);
asserts.isequal(mod(0,-Inf), -0);
asserts.isequal(mod(6.0,-2), -0);
asserts.isequal(mod(true,3), 1);
asserts.isequal(mod('a',3), 1);
asserts.isequal(mod(5,true), 0);
asserts.isequal(mod(-0.3,-0.1), -0);
asserts.isequal(mod([5 -5 6 -6],[3 3 -3 -3]), [2 1 -0 -0]);
asserts.isequal(mod([0 -0 6 -6],-3), [-0 -0 -0 -0]);
asserts.isequal(mod(single(6),single(-3)), single(-0));
asserts.isequal(mod(single(0),single(-3)), single(-0));
asserts.isequal(mod(-6,[3 -3]), [0 -0]);
asserts.isequal(mod([true false],2), [1 0]);
asserts.isequal(mod('ab',2), [1 0]);
asserts.isequal(mod(true,true), 0);
asserts.isequal(mod(false,true), 0);
asserts.isequal(mod(true,false), 1);
asserts.isequal(mod('a','b'), 97);
asserts.isequal(mod(4,-2), -0);
asserts.isequal(mod(-4,-2), -0);
asserts.isequal(mod(4.5,-1.5), -0);
asserts.isequal(mod(-4.5,-1.5), -0);
asserts.isequal(mod(0.5,-0.5), -0);
asserts.isequal(mod(-0.5,-0.5), -0);
asserts.isequal(1 ./ mod(0,-3), -Inf);
asserts.isequal(1 ./ mod(6,-3), -Inf);
asserts.isequal(1 ./ mod(single(0),single(-3)), single(-Inf));
asserts.isequal(1 ./ mod([0 6 -6 0],[-3 -3 3 3]), [-Inf -Inf Inf Inf]);
%=============================================================================
