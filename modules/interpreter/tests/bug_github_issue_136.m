%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/134
% <-- Short Description -->
% Fix precedence of Colon ":" operator.
%=============================================================================
asserts.isequal(5:10 == 5:10, logical(ones(1, 6)));
asserts.isequal([5:10] == [5:10], logical(ones(1, 6)));
asserts.isequal((5:10) == (5:10), logical(ones(1, 6)));
asserts.isequal(5:10 == [5:10], logical(ones(1, 6)));
asserts.isequal([5:10] == 5:10, logical(ones(1, 6)));
asserts.isequal([5:10] == (5:10), logical(ones(1, 6)));
asserts.isequal((5:10) == [5:10], logical(ones(1, 6)));
%=============================================================================
