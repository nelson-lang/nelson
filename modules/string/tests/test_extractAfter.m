%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(extractAfter('abc.def', '.'), 'def');
asserts.isequal(extractAfter('abcdef', 3), 'def');
asserts.isequal(extractAfter(["a:b", "c:d"], ":"), ["b", "d"]);
asserts.isequal(extractAfter("你好世界", "你好"), "世界");
%=============================================================================
