%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'b', 'a', 'b', '', 'c'});
B = categorical({'b', 'd'});
[tf, loc] = ismember(A, B);
asserts.isequal(tf, [true false true false false]);
asserts.isequal(loc, [1 0 1 0 0]);
[tf, loc] = ismember(categorical({'b', 'z'}), A);
asserts.isequal(tf, [true false]);
asserts.isequal(loc, [1 0]);
%=============================================================================
[C, ia] = setdiff(A, B);
asserts.isequal(cellstr(C), {'a', '<undefined>', 'c'});
asserts.isequal(ia(:), [2; 4; 5]);
[C, ia, ib] = intersect(A, B);
asserts.isequal(cellstr(C), {'b', 'b'});
asserts.isequal(ia(:), [1; 3]);
asserts.isequal(ib(:), [1; 1]);
[C, ia, ib] = setxor(A, B);
asserts.isequal(cellstr(C), {'a'; '<undefined>'; 'c'; 'd'});
asserts.isequal(ia(:), [2; 4; 5]);
asserts.isequal(ib(:), 2);
%=============================================================================
[C, ia, ib] = union(categorical({'b', 'a'}), categorical({'c', 'a'}));
asserts.isequal(cellstr(C), {'a'; 'b'; 'c'});
asserts.isequal(ia(:), [2; 1]);
asserts.isequal(ib(:), 1);
[C, ia, ic] = unique(categorical({'b', 'a', 'b', ''}));
asserts.isequal(cellstr(C), {'a'; 'b'; '<undefined>'});
asserts.isequal(ia(:), [2; 1; 4]);
asserts.isequal(ic(:), [2; 1; 2; 3]);
[C, ia, ic] = unique(categorical({'b', 'a', 'b', ''}), 'stable');
asserts.isequal(cellstr(C), {'b'; 'a'; '<undefined>'});
asserts.isequal(ia(:), [1; 2; 4]);
%=============================================================================
