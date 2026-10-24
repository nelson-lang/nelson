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
asserts.isequal(p.Status, "NotLoaded");
R = strtrim(evalc('pyrun(["greeting = ''hello''", "print(greeting)"])'));
asserts.isequal(R, 'hello');
asserts.isequal(p.Status, "Loaded");
%=============================================================================
code = ["import math";
  "x = 4.2";
  "n = 3";
  "power = x ** n"];
power = pyrun(code, 'power');
asserts.isequal(power, 4.2 ^ 3);
%=============================================================================
C = pyrun('A', 'A', 'A', 1);
asserts.isequal(C, 1);
%=============================================================================
asserts.checkerror('C = pyrun(''A = 1'', ''A'', 1, ''A'')', _('Field names must be string scalars or character vectors.'));
%=============================================================================
PYCODE = pyrun('x = compile(''print(55)'', ''test'', ''eval'')', 'x');
asserts.isequal(class(PYCODE), 'py.code');
R = strtrim(evalc('pyrun(PYCODE)'));
asserts.isequal(R, '55');
%=============================================================================
PYCODE = pyrun('X = compile(''Y = 3'', ''test'', ''exec'')', 'X');
asserts.isequal(class(PYCODE), 'py.code');
R = pyrun(PYCODE, 'Y');
asserts.isequal(R.char(), '3');
%=============================================================================