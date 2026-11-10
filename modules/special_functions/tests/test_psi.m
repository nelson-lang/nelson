%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isapprox(psi([0.5 1 2 10 100 1e-3]), [-1.96351002602142 -0.577215664901532 0.422784335098467 2.25175258906672 4.60016185273809 -1000.57557193181], 1e-14);
assert_isequal(psi(0), -Inf);
assert_isequal(psi([Inf NaN]), [Inf NaN]);
assert_isapprox(psi(1, [0.5 1 2 10]), [4.93480220054468 1.64493406684823 0.644934066848226 0.105166335681686], 1e-14);
assert_isapprox(psi(2, [0.5 1 2 10]), [-16.8287966442343 -2.40411380631919 -0.404113806319189 -0.0110498349708021], 1e-14);
assert_isapprox(psi(3, [1 2.5]), [6.49393940226683 0.223905848817252], 1e-14);
assert_isapprox(psi(10, 3), -21.436606628713, 1e-13);
assert_isequal(psi(0, 2), psi(2));
assert_isequal(psi(1, [0 Inf]), [Inf Inf]);
assert_isequal(size(psi(ones(2, 3))), [2 3]);
R = psi(single([1 2]));
assert_isequal(class(R), 'single');
assert_isapprox(R, single([-0.577215671539307 0.422784328460693]), 1e-6);
assert_checkerror('psi(-0.5)', _('X must be nonnegative.'));
assert_checkerror('psi(1i)', _('X must be real.'));
assert_checkerror('psi(int8(2))', _('X must be a single or double array.'));
assert_checkerror('psi([0 1], 2)', _('K must be a nonnegative integer scalar.'));
%=============================================================================
