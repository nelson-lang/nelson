%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(extractBefore('abc.def', '.'), 'abc');
asserts.isequal(extractBefore('abcdef', 4), 'abc');
asserts.isequal(extractBefore(["a:b", "c:d"], ":"), ["a", "c"]);
asserts.isequal(extractBefore("你好世界", "世界"), "你好");
%=============================================================================
