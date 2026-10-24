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
R = split("a,b,c", ",");
asserts.isequal(R, ["a"; "b"; "c"]);
C = split('a b');
asserts.isequal(C, {'a'; 'b'});
names = ["Mary Butler"; "Santiago Marquez"; "Diana Lee"];
asserts.isequal(split(names, " ", 1), ["Mary", "Santiago", "Diana"; "Butler", "Marquez", "Lee"]);
[parts, matches] = split("bacon, lettuce, and tomato", ["and", ",", " "]);
asserts.isequal(parts, ["bacon"; ""; "lettuce"; ""; ""; ""; "tomato"]);
asserts.isequal(matches, [","; " "; ","; " "; "and"; " "]);
asserts.isequal(split("北京,上海,深圳", ","), ["北京"; "上海"; "深圳"]);
%=============================================================================
