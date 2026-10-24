%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(angle(sparse([1 -1])), sparse([0 3.1415926535897931]));
asserts.isequal(angle(-0), 0);
asserts.isequal(angle(sparse(1+1i)), sparse(0.78539816339744828));
asserts.isequal(angle([true false]), [0 0]);
asserts.isequal(angle('ab'), [0 0]);
asserts.isequal(angle(sparse([0 -1 2i])), sparse([0 3.1415926535897931 1.5707963267948966]));
asserts.isequal(angle(complex(-0,0)), 0);
asserts.isequal(angle(complex(-0,-0)), 0);
asserts.isequal(angle(complex(0,-0)), 0);
asserts.isequal(angle(single(-0)), single(0));
asserts.isequal(angle([-0 0]), [0 0]);
asserts.isequal(1 ./ angle(-0), Inf);
asserts.isequal(1 ./ angle(complex(-0,-0)), Inf);
asserts.isequal(1 ./ angle(complex(0,-0)), Inf);
%=============================================================================
