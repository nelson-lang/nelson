%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[s, w] = system(["echo hello", "echo my", "echo world"]);
assert_isequal(s, [0 0 0]);
assert_isequal(size(w), [1 3]);
assert_istrue(isa(w, 'string'));
w1 = char(w(1));
w1 = w1(1:5);
w2 = char(w(2));
w2 = w2(1:2);
w3 = char(w(3));
w3 = w3(1:5);
assert_isequal(w1, 'hello')
assert_isequal(w2, 'my');
assert_isequal(w3, 'world');
%=============================================================================
