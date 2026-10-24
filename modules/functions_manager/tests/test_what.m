%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('what'), 0);
asserts.isequal(nargout('what'), -1);
%=============================================================================
[builtinList, macroList] = what();
asserts.istrue(iscell(builtinList));
asserts.istrue(iscell(macroList));
asserts.istrue(any(strcmp(builtinList, 'what')));
asserts.istrue(any(strcmp(macroList, 'test_run')));
%=============================================================================
matrix = common_functions_matrix();
asserts.istrue(isstruct(matrix));
asserts.istrue(length(matrix) >= 180);
asserts.isequal(fieldnames(matrix), {'name'; 'area'; 'status'; 'tier'});
[builtinList, macroList] = what();
asserts.istrue(iscell(builtinList));
asserts.istrue(iscell(macroList));
asserts.istrue(any(strcmp(builtinList, 'what')));
asserts.istrue(any(strcmp(macroList, 'test_run')));
%=============================================================================
