%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(extractBetween("abcDEFghi", 3, 7), "cDEFg");
asserts.isequal(extractBetween("a[bc]d", "[", "]"), "bc");
asserts.isequal(extractBetween("a<one>b<two>", "<", ">"), ["one"; "two"]);
asserts.isequal(extractBetween("a[bc]d", "[", "]", "Boundaries", "inclusive"), "[bc]");
S = ["one <a>"; "two [b]"];
asserts.isequal(extractBetween(S, ["<"; "["], [">"; "]"]), ["a"; "b"]);
%=============================================================================
s = ["a[x]b", missing, "c[y]d"];
r = extractBetween(s, "[", "]");
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "x");
asserts.isequal(r(3), "y");
%=============================================================================
