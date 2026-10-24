%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = [0 0; 1 0; 1 1; 0 1];
t = delaunayn(p);
q = [0.25 0.25; 2 2];
[idx, bary] = tsearchn(p, t, q);
asserts.istrue(~isnan(idx(1)));
asserts.istrue(isnan(idx(2)));
asserts.isapprox(sum(bary(1, :)), 1, 1e-12);
%=============================================================================
