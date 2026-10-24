%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
yq = spline(1:4, [0 1 0 1], [1.5 2.5]);
asserts.isapprox(yq, [1 0.5], 1e-12);
pp = spline(1:4, [0 1 0 1]);
asserts.isequal(pp.form, 'pp');
asserts.isequal(pp.breaks, 1:4);
asserts.isequal(pp.pieces, 3);
asserts.isequal(pp.order, 4);
asserts.isequal(size(pp.coefs), [3 4]);
%=============================================================================
