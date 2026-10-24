%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(insertAfter('abc', 2, 'XX'), 'abXXc');
asserts.isequal(insertAfter("a=b", "=", "XX"), "a=XXb");
S = ["a=b"; "c:d"];
asserts.isequal(insertAfter(S, ["="; ":"], ["1"; "2"]), ["a=1b"; "c:2d"]);
%=============================================================================
%=============================================================================
% Insertion happens at every match (literal and pattern), not only the first.
asserts.isequal(insertAfter("a1a2", "a", "Z"), "aZ1aZ2");
asserts.isequal(insertAfter("a1b2c3", digitsPattern, "X"), "a1Xb2Xc3X");
asserts.isequal(insertAfter("1a2b", lettersPattern, "!"), "1a!2b!");
%=============================================================================
s = ["abc", missing, "xyz"];
r = insertAfter(s, "a", "-");
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "a-bc");
%=============================================================================
