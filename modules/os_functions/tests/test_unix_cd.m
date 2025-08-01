%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
cmd = 'cd';
[a, b] = unix(cmd);
assert_isequal(a, 0);
%=============================================================================
cmd = 'cd';
[a, b] = system(cmd);
assert_isequal(a, 0);
%=============================================================================
cmd = 'cd';
[a, b] = dos(cmd);
assert_isequal(a, 0);
%=============================================================================
