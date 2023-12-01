%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/528
% <-- Short Description -->
% Assignment in cell did not work in this case [c{:}] = ind2sub (dv, i)
%=============================================================================
dv = [2    3];
i = 2;
R = cell (size (dv));
[R{:}] = ind2sub (dv, i);
REF = {2, 1};
assert_isequal(R, REF)
%=============================================================================
