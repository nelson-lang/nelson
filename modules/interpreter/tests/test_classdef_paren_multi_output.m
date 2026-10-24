%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Multi-output "()" indexing of a class object must dispatch to subsref with
% the correct nargout (driven by the number of left-hand-side targets).
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
obj = ClassdefParenMultiOutput([10 20 30 40]);
%=============================================================================
% single output still works
a = obj([2 4]);
asserts.isequal(a, 20);
%=============================================================================
% two outputs -> subsref called with nargout == 2
[a, b] = obj([1 3]);
asserts.isequal(a, 10);
asserts.isequal(b, 30);
%=============================================================================
% three outputs
[a, b, c] = obj([1 2 4]);
asserts.isequal(a, 10);
asserts.isequal(b, 20);
asserts.isequal(c, 40);
%=============================================================================
% plain function handles keep their multi-output behaviour
f = @(x) deal(x, x * 2);
[u, v] = f(7);
asserts.isequal(u, 7);
asserts.isequal(v, 14);
%=============================================================================
