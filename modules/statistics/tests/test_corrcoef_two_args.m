%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% corrcoef(x, y) concatenates x(:) and y(:) as two columns
asserts.isapprox(corrcoef([1 2 3], [2 4 6]), ones(2, 2), 1e-12);
asserts.isapprox(corrcoef([1 2 3], [6 4 2]), [1 -1; -1 1], 1e-12);
R1 = corrcoef([1 2 4 8], [1 3 2 9]);
R2 = corrcoef([[1; 2; 4; 8], [1; 3; 2; 9]]);
asserts.isapprox(R1, R2, 1e-12);
% column/row mix
asserts.isapprox(corrcoef([1; 2; 3], [6 4 2]), [1 -1; -1 1], 1e-12);
%=============================================================================
% one argument form still works
asserts.isapprox(corrcoef([1 2; 2 4; 3 6]), ones(2, 2), 1e-12);
%=============================================================================
