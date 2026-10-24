%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Complex integers are not supported: an integer concatenated with a value
% whose imaginary part is not zero in the integer class raises an error
% instead of losing the imaginary part.
%=============================================================================
% the imaginary part converts to zero: the result is a real integer
R = [int8(1); complex(1, 0)];
assert_isequal(R, int8([1; 1]));
assert_istrue(isreal(R));
R = [int8(1), complex(1, 0.4)];
assert_isequal(R, int8([1 1]));
R = [int8(1), NaN * 1i];
assert_isequal(R, int8([1 0]));
R = [uint8(1), complex(-0.4, -7)];
assert_isequal(R, uint8([1 0]));
R = [int8(1), complex([])];
assert_isequal(R, int8(1));
R = cat(3, int8(1), complex(2, 0));
assert_isequal(R, cat(3, int8(1), int8(2)));
%=============================================================================
% a cell keeps each value as it is
R = [int8(1), {1i}];
assert_isequal(R, {int8(1), 1i});
%=============================================================================
id = 'Nelson:concatenation:complexIntegerUnsupported';
msg = _('Complex integers are not supported: an integer cannot be concatenated with a value that has a nonzero imaginary part.');
assert_checkerror('[int8(1), 1i]', msg, id);
assert_checkerror('[1i, int8(1)]', msg, id);
assert_checkerror('[int8(1); 1i]', msg, id);
assert_checkerror('[uint16(2), single(1i)]', msg, id);
assert_checkerror('[int8(1), complex(300, -300)]', msg, id);
assert_checkerror('[int8(1), complex(1, 0), 2i]', msg, id);
assert_checkerror('[int8([1 2]); 1i 2]', msg, id);
assert_checkerror('[int16(1), int8(1), 1i]', msg, id);
assert_checkerror('[int8(1), true, 1i]', msg, id);
assert_checkerror('[uint8(1), complex(0, 0.5)]', msg, id);
assert_checkerror('horzcat(int8(1), 1i)', msg, id);
assert_checkerror('vertcat(int8(1), 1i)', msg, id);
assert_checkerror('cat(1, int8(1), 1i)', msg, id);
assert_checkerror('cat(3, int8(1), 1i)', msg, id);
assert_checkerror('cat(3, int64([1 2]), [1i 2])', msg, id);
%=============================================================================
