%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical(["red" "blue" "green" "blue" "red"], ["red" "green" "blue"]);
asserts.isequal(countcats(A), [2 1 2]);
A = categorical([1 3; 2 1; 3 1], 1:3, ["red" "green" "blue"]);
asserts.isequal(countcats(A), [1 2; 1 0; 1 1]);
%=============================================================================
