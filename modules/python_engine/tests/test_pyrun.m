%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
p = pyenv();
assert_isequal(p.Status, "NotLoaded");
R = strtrim(evalc('pyrun(["greeting = ''hello''", "print(greeting)"])'));
assert_isequal(R, 'hello');
assert_isequal(p.Status, "Loaded");
%=============================================================================
code = ["import math";
"x = 4.2";
"n = 3";
"power = x ** n"];
power = pyrun(code, 'power');
assert_isequal(power, 4.2^3);
%=============================================================================
C = pyrun('A', 'A', 'A', 1);
assert_isequal(C, 1);
%=============================================================================
assert_checkerror('C = pyrun(''A = 1'', ''A'', 1, ''A'')', _('Field names must be string scalars or character vectors.'));
%=============================================================================
PYCODE = pyrun('x = compile(''print(55)'', ''test'', ''eval'')', 'x');
assert_isequal(class(PYCODE), 'py.code');
R = strtrim(evalc('pyrun(PYCODE)'));
assert_isequal(R, '55');
%=============================================================================
PYCODE = pyrun('X = compile(''Y = 3'', ''test'', ''exec'')', 'X');
assert_isequal(class(PYCODE), 'py.code');
R = pyrun(PYCODE, 'Y');
assert_isequal(R.char(), '3');
%=============================================================================