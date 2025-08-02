%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_ref = modulepath('dynamic_link', 'builtin');
lib = dlopen(path_ref);
R = [lib; lib];
assert_isequal(size(R),  [2 1]);
assert_isequal(class(R), 'dllib');
%=============================================================================
