%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = [1 2 3 4 5]; y = [1 4 9 16 25];
asserts.isapprox(pchip(x, y, 2.5), 6.239583, 1e-5);
asserts.isapprox(pchip(x, y, 6), 35.75, 1e-5);
pp = pchip(x, y);
asserts.isequal(class(pp), 'struct');
asserts.isapprox(ppval(pp, 3.5), 12.244792, 1e-5);
%=============================================================================
