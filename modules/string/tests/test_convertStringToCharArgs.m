%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('convertStringToCharArgs'), 1);
asserts.isequal(nargout('convertStringToCharArgs'), 1);
%=============================================================================
R = convertStringToCharArgs({"hello", 'world', ["this", "is"], {'a', 'test'}});
REF = {'hello', 'world', ["this", "is"], {'a', 'test'}};
asserts.isequal(R, REF);
%=============================================================================
R = convertStringToCharArgs("single string");
REF = 'single string';
asserts.isequal(R, REF);
%=============================================================================
R = convertStringToCharArgs({'mixed', "cell", 'of', "strings"});
REF = {'mixed', 'cell', 'of', 'strings'};
asserts.isequal(R, REF);
%=============================================================================
R = convertStringToCharArgs({42, "number", 'test'});
REF = {42, 'number', 'test'};
asserts.isequal(R, REF);
%=============================================================================
R = convertStringToCharArgs({["array", "of", "strings"], 'and', "char"});
REF = {["array", "of", "strings"], 'and', 'char'};
asserts.isequal(R, REF);
%=============================================================================
