%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical(["red" "green" "blue" "red"], ["blue" "green" "red"]);
B = reordercats(A, ["red" "green" "blue"]);
asserts.isequal(categories(B), {'red'; 'green'; 'blue'});
asserts.isequal(cellstr(B), {'red', 'green', 'blue', 'red'});
%=============================================================================
