%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = extract('a12b34', digitsPattern(2));
asserts.isequal(R, {'12'; '34'});
R = extract("abc123", regexpPattern('\d+'));
asserts.isequal(R{1}, '123');
asserts.isequal(extract("abc", 2), "b");
R = extract("a12b34", digitsPattern(2));
asserts.isequal(string(R), ["12"; "34"]);
R = extract("Hello World", regexpPattern('. *'));
asserts.isequal(size(R), [10 1]);
asserts.isequal(R, ["H"; "e"; "l"; "l"; "o "; "W"; "o"; "r"; "l"; "d"]);
asserts.isequal(extract("你好世界", 2), "好");
%=============================================================================
