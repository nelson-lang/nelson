%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A classdef method building a matrix that mixes string literals and function
% calls separated by whitespace must not break class registration: the space
% before a quote is a matrix-element separator, not a transpose operator.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
obj = ClassdefMethodStringMatrix([2 3 4]);
assert_isequal(class(obj), 'ClassdefMethodStringMatrix');
%=============================================================================
assert_isequal(obj.render(), '2 << 3, 4 >>');
%=============================================================================
