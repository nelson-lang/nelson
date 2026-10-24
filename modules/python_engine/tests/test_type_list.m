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
R = pyrun("A = [1, 2, 3, 4]", 'A');
asserts.isequal(class(R), 'py.list')
TXT = evalc('R');
asserts.istrue(contains(TXT, 'Python list with values:'));
asserts.isfalse(contains(TXT, 'Python %s'));
RC = R.cell();
asserts.isequal(size(RC), [1, 4])
asserts.isequal(RC{1}.double(), 1);
asserts.isequal(RC{2}.double(), 2);
asserts.isequal(RC{3}.double(), 3);
asserts.isequal(RC{4}.double(), 4);
%=============================================================================
asserts.isequal(R.double(), double([1, 2, 3, 4]))
%=============================================================================
asserts.isequal(R.string(), ["1", "2", "3", "4"])
%=============================================================================
asserts.isequal(R.single(), single([1, 2, 3, 4]))
%=============================================================================
asserts.isequal(R.int8(), int8([1, 2, 3, 4]))
asserts.isequal(R.int16(), int16([1, 2, 3, 4]))
asserts.isequal(R.int32(), int32([1, 2, 3, 4]))
asserts.isequal(R.int64(), int64([1, 2, 3, 4]))
%=============================================================================
asserts.isequal(R.uint8(), uint8([1, 2, 3, 4]))
asserts.isequal(R.uint16(), uint16([1, 2, 3, 4]))
asserts.isequal(R.uint32(), uint32([1, 2, 3, 4]))
asserts.isequal(R.uint64(), uint64([1, 2, 3, 4]))
%=============================================================================
R = pyrun("A = [1, '2', 3, 4]", 'A');
asserts.isequal(R.string(), ["1", "2", "3", "4"])
asserts.checkerror('R.double()', _('All Python elements must be convertible as scalar to the requested type.'))
%=============================================================================
R = py.list({'This', 'is a', 'list'});
TXT = evalc('R');
asserts.istrue(contains(TXT, 'Python list with values:'));
asserts.istrue(contains(TXT, '[''This'', ''is a'', ''list'']'));
%=============================================================================
R = py.textwrap.wrap('This is a string');
asserts.isequal(class(R), 'py.list');
TXT = evalc('R');
asserts.istrue(contains(TXT, '[''This is a string'']'));
%=============================================================================
