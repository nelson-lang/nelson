%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a new string variable assigned on some rows is padded with <missing>
%=============================================================================
warning('off', 'Nelson:table:rowsAddedNewVars');
T = table([1; 2; 3]);
T{1, 'New'} = "x";
assert_isequal(class(T.New), 'string');
assert_isequal(ismissing(T.New), [false; true; true]);
assert_isequal(T.New(1), "x");
%=============================================================================
T = table([1; 2; 3]);
T{[1 3], 'New'} = ["x"; "y"];
assert_isequal(ismissing(T.New), [false; true; false]);
%=============================================================================
T = table([1; 2; 3]);
T{1, {'A', 'B'}} = ["x" "y"];
assert_isequal(ismissing(T.B), [false; true; true]);
%=============================================================================
T = table([1; 2]);
T(1, 'S') = {"q"};
assert_isequal(ismissing(T.S), [false; true]);
%=============================================================================
warning('on', 'Nelson:table:rowsAddedNewVars');
%=============================================================================
