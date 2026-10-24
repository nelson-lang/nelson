%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('feval'), -1);
asserts.isequal(nargout('feval'), -1);
%=============================================================================
asserts.isequal(feval('cos', 0), 1);
asserts.isequal(feval(str2func('cos'), 0), 1);
[r, c] = feval('size', ones(2, 3));
asserts.isequal(r, 2);
asserts.isequal(c, 3);
%=============================================================================
sumHandle = str2func('sum');
asserts.isequal(feval(sumHandle, [1 2 3]), 6);
asserts.isequal(feval('cos', 0), 1);
[rowCount, columnCount] = feval('size', ones(2, 3));
asserts.isequal(rowCount, 2);
asserts.isequal(columnCount, 3);
%=============================================================================
