%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = 3.5;
asserts.isequal(class(x), 'double');
asserts.istrue(isscalar(x));
asserts.isequal(size(x), [1 1]);
%=============================================================================
y = x(:);
asserts.isequal(y, 3.5);
asserts.istrue(iscolumn(y));
%=============================================================================
z = x(false);
asserts.istrue(isempty(z));
asserts.isequal(class(z), 'double');
%=============================================================================
copy = x;
x(1) = 4.5;
asserts.isequal(copy, 3.5);
asserts.isequal(x, 4.5);
%=============================================================================
res_cmd = 0;
interrupted = any(res_cmd(:) == -1073741510);
interrupted = any(interrupted(:));
asserts.isfalse(interrupted);
%=============================================================================
