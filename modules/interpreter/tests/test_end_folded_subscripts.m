%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% with fewer subscripts than dimensions, the last 'end' spans the trailing
% dimensions
x = reshape(1:24, 2, 3, 4);
asserts.isequal(x(end, end), 24);
asserts.isequal(x(1, end), 23);
asserts.isequal(x(end), 24);
asserts.isequal(x(end, end, end), 24);
asserts.isequal(x(2, 12), 24);
asserts.isequal(x(:, end), [23; 24]);
asserts.isequal(x(end, 1:end), 2:2:24);
%=============================================================================
