%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = sprandsym(20, 0.2);
assert_istrue(issparse(R));
assert_isequal(R, R.');
assert_isequal(size(R), [20 20]);
S = sparse([1 0 0; 2 3 0; 0 4 5]);
R = sprandsym(S);
assert_isequal(R, R.');
assert_isequal(full(R ~= 0), full((S + tril(S, -1).') ~= 0));
R = sprandsym(12, 0.3, 1e-2);
assert_isapprox(norm(full(R - R.')), 0, 1e-12);
e = eig(full(R));
assert_istrue(all(e > 0));
assert_isapprox(min(e) / max(e), 1e-2, 1e-8);
R = sprandsym(4, 0.5, [1 2 3 4]);
assert_isapprox(sort(eig(full(R))), [1; 2; 3; 4], 1e-12);
assert_checkerror('sprandsym(4, 0.5, 0.1, 3)', _('Only kinds 1 and 2 are supported.'));
%=============================================================================
