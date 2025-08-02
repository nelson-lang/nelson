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
assert_isequal(class(R), 'py.list')
RC = R.cell();
assert_isequal(size(RC), [1, 4])
assert_isequal(RC{1}.double(), 1);
assert_isequal(RC{2}.double(), 2);
assert_isequal(RC{3}.double(), 3);
assert_isequal(RC{4}.double(), 4);
%=============================================================================
assert_isequal(R.double(), double([1, 2, 3, 4]))
%=============================================================================
assert_isequal(R.string(), ["1", "2", "3", "4"])
%=============================================================================
assert_isequal(R.single(), single([1, 2, 3, 4]))
%=============================================================================
assert_isequal(R.int8(), int8([1, 2, 3, 4]))
assert_isequal(R.int16(), int16([1, 2, 3, 4]))
assert_isequal(R.int32(), int32([1, 2, 3, 4]))
assert_isequal(R.int64(), int64([1, 2, 3, 4]))
%=============================================================================
assert_isequal(R.uint8(), uint8([1, 2, 3, 4]))
assert_isequal(R.uint16(), uint16([1, 2, 3, 4]))
assert_isequal(R.uint32(), uint32([1, 2, 3, 4]))
assert_isequal(R.uint64(), uint64([1, 2, 3, 4]))
%=============================================================================
R = pyrun("A = [1, '2', 3, 4]", 'A');
assert_isequal(R.string(), ["1", "2", "3", "4"])
assert_checkerror('R.double()', _('All Python elements must be convertible as scalar to the requested type.'))
%=============================================================================
