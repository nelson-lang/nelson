%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('exist'), -1);
assert_isequal(nargout('exist'), 1);
%=============================================================================
r = exist('toto');
assert_isequal(r, 0);
toto = 3;
assert_isequal(exist('toto'), 1);
clear toto
assert_isequal(exist('toto'), 0);
assert_isequal(exist('toto', 'builtin'), 0);
assert_isequal(exist('exist', 'builtin'), 0);
assert_isequal(exist('who', 'builtin'), 5);
assert_isequal(exist('exist', 'file'), 2);
assert_isequal(exist('exist'), 2);
assert_isequal(exist(nelsonroot(), 'dir'), 7);
%=============================================================================
