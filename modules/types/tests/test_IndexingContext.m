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
c = nelson.mixin.util.IndexingContext.Expression;
asserts.isequal(class(c), 'nelson.mixin.util.IndexingContext');
asserts.isequal(char(c), 'Expression');
%=============================================================================
m = enumeration('nelson.mixin.util.IndexingContext');
asserts.isequal(numel(m), 3);
names = arrayfun(@char, m, 'UniformOutput', false);
asserts.istrue(any(strcmp(names, 'Statement')));
asserts.istrue(any(strcmp(names, 'Expression')));
asserts.istrue(any(strcmp(names, 'Assignment')));
%=============================================================================
asserts.istrue(c == nelson.mixin.util.IndexingContext.Expression);
asserts.isfalse(c == nelson.mixin.util.IndexingContext.Statement);
asserts.isequal(char(nelson.mixin.util.IndexingContext.Assignment), 'Assignment');
%=============================================================================
