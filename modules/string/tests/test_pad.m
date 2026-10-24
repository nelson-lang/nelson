%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(pad('abc', 5), 'abc  ');
asserts.isequal(pad('abc', 5, 'left', '0'), '00abc');
asserts.isequal(pad("abc", 5, "both", "_"), "_abc_");
names = ["Mary"; "Elizabeth"; "James"];
asserts.isequal(pad(names, "left"), ["     Mary"; "Elizabeth"; "    James"]);
asserts.isequal(pad("中", 3, "both", "好"), "好中好");
%=============================================================================
s = ["a", missing, "cc"];
r = pad(s, 3);
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "a  ");
asserts.isequal(r(3), "cc ");
%=============================================================================
