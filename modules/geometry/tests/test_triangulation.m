%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tr = triangulation([1 2 3], [0 0; 2 0; 0 2]);
asserts.isequal(size(edges(tr), 2), 2);
[loc, bc] = pointLocation(tr, [0.25 0.25]);
asserts.istrue(~isnan(loc));
asserts.isapprox(sum(bc), 1, 1e-12);
asserts.isapprox(incenter(tr), [2 - sqrt(2), 2 - sqrt(2)], 1e-12);
%=============================================================================
