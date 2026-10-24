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
asserts.isequal(insertBefore('abc', 2, 'XX'), 'aXXbc');
asserts.isequal(insertBefore("a=b", "=", "XX"), "aXX=b");
S = ["a=b"; "c:d"];
asserts.isequal(insertBefore(S, ["="; ":"], ["1"; "2"]), ["a1=b"; "c2:d"]);
%=============================================================================
% Insertion happens at every match (literal and pattern), not only the first.
asserts.isequal(insertBefore("a1a2", "a", "Z"), "Za1Za2");
asserts.isequal(insertBefore("1a2b3", digitsPattern, "-"), "-1a-2b-3");
%=============================================================================
s = ["abc", missing, "xyz"];
r = insertBefore(s, 2, "-");
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "a-bc");
asserts.isequal(r(3), "x-yz");
%=============================================================================
