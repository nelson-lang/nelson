%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isapprox(legendre(2, [0 0.5 1]), [-0.5 -0.125 1; 0 -1.29903810567666 0; 3 2.25 0], 1e-14);
assert_isequal(legendre(0, [0.2 0.3]), [1 1]);
assert_isapprox(legendre(3, [0.1 0.7], 'sch'), [-0.1475 -0.1925; -0.578837736330312 0.634116018564426; 0.191712675637267 0.691327527298024; 0.778740569766338 0.287935539661223], 1e-14);
assert_isapprox(legendre(3, [0.1 0.7], 'norm'), [-0.275947232274578 -0.360134523476992; -0.765730349894791 0.838856643741945; 0.2536120314575 0.914540355861894; 1.03017694171924 0.380902915780386], 1e-14);
assert_isequal(size(legendre(2, ones(2, 3) / 2)), [3 2 3]);
assert_isapprox(legendre(2, 0.3), [-0.365; -0.858545281275251; 2.73], 1e-14);
assert_isapprox(legendre(5, 0.3), [0.34538625; 0.160798376638844; -10.462725; 8.65914461606197; 234.76635; -746.509414795237], 1e-13);
assert_isequal(class(legendre(2, single(0.5))), 'single');
P = legendre(200, [-0.3 0.4], 'norm');
assert_istrue(all(isfinite(P(:))));
assert_checkerror('legendre(2, 1.5)', _('X must be real with values in [-1, 1].'));
assert_checkerror('legendre(-1, 0.5)', _('N must be a nonnegative integer scalar.'));
assert_checkerror('legendre(2, 0.5, ''foo'')', _('Normalization must be ''unnorm'', ''sch'' or ''norm''.'));
%=============================================================================
