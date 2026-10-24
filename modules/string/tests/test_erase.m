%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(erase('abcabc', 'b'), 'acac');
asserts.isequal(erase("abcabc", "bc"), "aa");
asserts.isequal(erase({'abc', 'bbb'}, 'b'), {'ac', ''});
asserts.isequal(erase("one two three", ["one", "three"]), " two ");
asserts.isequal(erase("你好世界", "世界"), "你好");
asserts.isequal(erase({'中文测试', '测试中文'}, '测试'), {'中文', '中文'});
%=============================================================================
s = ["aXa", missing, "c"];
r = erase(s, "a");
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "X");
asserts.isequal(r(3), "c");
%=============================================================================
