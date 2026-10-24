%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), 'Requires a native shell and process creation, unavailable in WebAssembly.');
[s, w] = system(["echo hello", "echo my", "echo world"]);
asserts.isequal(s, [0 0 0]);
asserts.isequal(size(w), [1 3]);
asserts.istrue(isa(w, 'string'));
w1 = char(w(1));
w1 = w1(1:5);
w2 = char(w(2));
w2 = w2(1:2);
w3 = char(w(3));
w3 = w3(1:5);
asserts.isequal(w1, 'hello')
asserts.isequal(w2, 'my');
asserts.isequal(w3, 'world');
%=============================================================================
