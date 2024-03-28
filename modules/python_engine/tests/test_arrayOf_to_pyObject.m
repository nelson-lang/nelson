%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
C = pyrun('', 'A', 'A', 1);
assert_isequal(C, 1);
%=============================================================================
C = pyrun('', 'A', 'A', []);
assert_isequal(C.char(), 'array(''d'')');
%=============================================================================
C = pyrun('', 'A', 'A', [1 2]);
assert_isequal(class(C), 'py.array.array');
%=============================================================================
M = methods(C);
E = {'double', 'int8', 'uint8', 'numeric', 'int16', 'logical', 'uint16', 'char', 'int32', 'uint32', 'int64', 'single', 'uint64'};
assert_istrue(all(ismember(E, M)));
%=============================================================================
assert_isequal(C.double(), [1 2]);
assert_isequal(C.single(), single([1 2]));
assert_isequal(C.int8(), int8([1 2]));
assert_isequal(C.uint8(), uint8([1 2]));
assert_isequal(C.numeric(), [1 2]);
assert_isequal(C.int16(), int16([1 2]));
assert_isequal(C.uint16(), uint16([1 2]));
assert_isequal(C.logical(), logical([1 2]));
assert_isequal(C.char(), 'array(''d'', [1.0, 2.0])');
assert_isequal(C.int32(), int32([1 2]));
assert_isequal(C.uint32(), uint32([1 2]));
assert_isequal(C.int64(), int64([1 2]));
assert_isequal(C.uint64(), uint64([1 2]));
%=============================================================================
assert_isequal(properties(C), {'itemsize'; 'typecode'})
%=============================================================================
C = pyrun('', 'A', 'A', [1 2]);
assert_isequal(C.char(), 'array(''d'', [1.0, 2.0])');
%=============================================================================
C = pyrun('', 'A', 'A', {1 2});
assert_isequal(C.char(),  '(1.0, 2.0)');
%=============================================================================
C = pyrun('', 'A', 'A', {1, 'nelson', 2});
assert_isequal(C.char(), '(1.0, ''nelson'', 2.0)');
%=============================================================================
