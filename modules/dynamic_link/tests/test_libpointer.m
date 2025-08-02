%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('libpointer'), -1);
assert_isequal(nargout('libpointer'), 1);
%=============================================================================
p = libpointer();
assert_istrue(p.isNull());
assert_isequal(p.DataType, '');
%=============================================================================
p = libpointer('int8');
assert_isfalse(p.isNull());
assert_isequal(p.DataType, 'int8');
assert_isequal(p.Value, int8(0));
%=============================================================================
assert_checkerror('p = libpointer(''int888'');', [_('Invalid argument type:'), 'int888']);
%=============================================================================
p = libpointer('int8Ptr');
assert_istrue(p.isNull());
assert_isequal(p.DataType, 'int8Ptr');
assert_checkerror('p.Value', _('The datatype and size of the value must be defined.'));
%=============================================================================
p = libpointer('int8', int8(3));
assert_isfalse(p.isNull());
assert_isequal(p.Value, int8(3));
assert_isequal(p.DataType, 'int8');
%=============================================================================
p = libpointer('int8Ptr', int8([3 4]));
assert_isfalse(p.isNull());
assert_isequal(p.DataType, 'int8Ptr');
assert_isequal(p.Value, int8([3 4]));
%=============================================================================
assert_checkerror('p = libpointer(''int8'', int8([3 4]));', _('Invalid #2 argument scalar expected.'));
%=============================================================================
str = 'This is a string';
voidptr = libpointer('voidPtr',[int8(str) int8(0)]);
assert_isequal(class(voidptr), 'libpointer');
assert_isequal(voidptr.DataType, 'voidPtr');
REF = int8([84, 104, 105, 115, 32, 105, 115, 32, 97,  32, 115, 116, 114, 105, 110, 103, 0]);
assert_isequal(voidptr.Value, REF);
assert_istrue(startsWith(char(voidptr.Value), str));
assert_isequal(length(char(voidptr.Value)), 17);
assert_isequal(length(str), 16);
%=============================================================================
x = 133.3;
xPtr = libpointer('doublePtr', x);
r = get(xPtr);
REF = struct();
REF.Value = x;
REF.DataType = 'doublePtr';
assert_isequal(r, REF);
%=============================================================================
path_ref = [modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()];
lib = dlopen(path_ref);
f = dlsym(lib, 'multiplicationDoubleByReference', 'libpointer', {'doublePtr'});
[r1, r2] = dlcall(f, xPtr);
%=============================================================================
r = get(xPtr);
REF.Value = x * 2;
REF.DataType = 'doublePtr';
assert_isequal(r, REF);
%=============================================================================
x = 133.3;
xPtr = libpointer('doublePtr', x);
r = get(xPtr);
r = xPtr.Value;
x = [1 2 3 4];
xPtr = libpointer('doublePtr', x);
xPtr.reshape(1, 1);
assert_isequal(xPtr.Value, 1);
xPtr.reshape(5, 5);
REF = [1     0     0     0     0;
2     0     0     0     0;
3     0     0     0     0;
4     0     0     0     0;
0     0     0     0     0];
assert_isequal(xPtr.Value, REF);
xPtr.reshape(1, 4);
assert_isequal(xPtr.Value, [1 2 3 4]);
xPtr.reshape(0, 0);
assert_isequal(xPtr.Value, []);
xPtr.reshape(1, 1);
assert_isequal(xPtr.Value, 1);
%=============================================================================
assert_checkerror('xPtr.reshape(-3, -1)', _('Expected a positive integer scalar.'));
%=============================================================================
