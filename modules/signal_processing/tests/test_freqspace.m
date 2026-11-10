%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(freqspace(4), [0 0.5 1]);
assert_isapprox(freqspace(5), [0 0.4 0.8], 1e-15);
assert_isequal(freqspace(4, 'whole'), [0 0.5 1 1.5]);
[f1, f2] = freqspace([3 4]);
assert_isequal(f1, [-1 -0.5 0 0.5]);
assert_isapprox(f2, [-2/3 0 2/3], 1e-15);
[f1, f2] = freqspace(4);
assert_isequal(f1, f2);
[x, y] = freqspace(3, 'meshgrid');
assert_isapprox(x, repmat([-2/3 0 2/3], 3, 1), 1e-15);
assert_isapprox(y, repmat([-2/3; 0; 2/3], 1, 3), 1e-15);
assert_checkerror('freqspace(4, ''half'')', _('Option must be ''whole'' or ''meshgrid''.'));
%=============================================================================
