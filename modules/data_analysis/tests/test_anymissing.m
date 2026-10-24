%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(anymissing([1 NaN 3]));
asserts.isfalse(anymissing([1 2 3]));
asserts.isfalse(anymissing([1 2; 3 4]));
asserts.istrue(anymissing(["a" missing]));
asserts.istrue(anymissing(table([1; NaN], 'VariableNames', {'v'})));
asserts.isequal(class(anymissing([1 NaN])), 'logical');
%=============================================================================
