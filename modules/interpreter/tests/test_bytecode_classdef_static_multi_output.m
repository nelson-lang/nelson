%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
[a, b, c] = ClassdefStaticMultiOutput.split(10);
asserts.isequal(a, 10);
asserts.isequal(b, 11);
asserts.isequal(c, 12);
%=============================================================================
[~, b, ~] = ClassdefStaticMultiOutput.split(20);
asserts.isequal(b, 21);
%=============================================================================
r = ClassdefStaticMultiOutput.vector(30)(2);
asserts.isequal(r, 31);
idx = {3};
r = ClassdefStaticMultiOutput.vector(30)(idx{:});
asserts.isequal(r, 32);
%=============================================================================
r = ClassdefPoint.origin().X;
asserts.isequal(r, 0);
%=============================================================================
