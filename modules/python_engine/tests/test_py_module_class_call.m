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
% py.module.ClassName(args): a class (uppercase leaf) called through the py
% proxy must instantiate it, exactly like py.module.function(args) does.
%=============================================================================
v = py.collections.OrderedDict();
assert_isequal(class(v), 'py.collections.OrderedDict');
%=============================================================================
v = py.decimal.Decimal('1.5');
assert_isequal(class(v), 'py.decimal.Decimal');
assert_isequal(double(v.__float__()), 1.5);
%=============================================================================
v = py.collections.OrderedDict(pyargs('a', 1));
assert_isequal(class(v), 'py.collections.OrderedDict');
assert_isequal(char(py.str(v)), 'OrderedDict({''a'': 1.0})');
%=============================================================================
% without parentheses still instantiates the class
v = py.collections.OrderedDict;
assert_isequal(class(v), 'py.collections.OrderedDict');
%=============================================================================
% lowercase leaf (function) keeps working
v = py.os.path.join('a', 'b');
assert_isequal(class(v), 'py.str');
v = py.collections.namedtuple('P', 'x y');
assert_isequal(class(v), 'py.type');
%=============================================================================
% inside a function file
addpath(modulepath('python_engine', 'tests'));
[d, p, f] = py_module_class_call_helper();
assert_isequal(class(d), 'py.collections.OrderedDict');
assert_isequal(class(p), 'py.str');
% a stdlib module function (lowercase leaf, scalar return) called through the
% py proxy must resolve identically in a compiled function body and a script
assert_isequal(class(f), 'py.int');
assert_isequal(double(f), 2);
assert_isequal(double(py.math.floor(2.5)), 2);
%=============================================================================
% unknown attribute still raises from the python proxy
assert_checkerror('py.collections.ordereddict()', 'Invalid property: ordereddict.');
%=============================================================================
