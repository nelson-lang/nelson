%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(replaceBetween('abcdef', 2, 5, 'XX'), 'aXXf');
asserts.isequal(replaceBetween("a[bc]d", "[", "]", "XX"), "a[XX]d");
asserts.isequal(eraseBetween("a[bc]d", "[", "]"), "a[]d");
S = ["a[bc]d"; "x<yz>w"];
asserts.isequal(replaceBetween(S, ["["; "<"], ["]"; ">"], ["1"; "2"]), ["a[1]d"; "x<2>w"]);
asserts.isequal(replaceBetween("语言[中文]测试", "[", "]", "汉字"), "语言[汉字]测试");
%=============================================================================
% Every start/stop pair is processed, not only the first.
asserts.isequal(replaceBetween("a[x]b[y]c", "[", "]", "Z"), "a[Z]b[Z]c");
asserts.isequal(replaceBetween("<a><b>", "<", ">", "T", 'Boundaries', 'inclusive'), "TT");
%=============================================================================
s = ["a[x]b", missing, "c[y]d"];
r = replaceBetween(s, "[", "]", "Z");
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "a[Z]b");
asserts.isequal(r(3), "c[Z]d");
%=============================================================================
