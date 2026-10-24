%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Empty operands take part in the class of a concatenation: the result class
% is decided by every operand, the values come from the nonempty ones.
%=============================================================================
R = [int8([]), 1];
assert_isequal(R, int8(1));
R = [1, int8([])];
assert_isequal(R, int8(1));
R = [int8([]); 1];
assert_isequal(R, int8(1));
R = [true, []];
assert_isequal(R, 1);
R = [[], true];
assert_isequal(R, 1);
R = [single([]), 1];
assert_isequal(R, single(1));
R = [single(zeros(0, 1)), true];
assert_isequal(R, single(1));
R = ['', 1];
assert_isequal(R, char(1));
R = ['', int8(65)];
assert_isequal(R, 'A');
R = [char(zeros(0, 3)), single(66)];
assert_isequal(R, 'B');
R = [uint16([]), int8(1)];
assert_isequal(R, uint16(1));
R = [int8(1), uint16([])];
assert_isequal(R, int8(1));
R = [true, int8([])];
assert_isequal(R, int8(1));
R = [NaN, [], ''];
assert_isequal(class(R), 'char');
R = [int8([]), 1, 2];
assert_isequal(R, int8([1 2]));
%=============================================================================
R = [complex(zeros(0, 0)), 1];
assert_isequal(class(R), 'double');
assert_isfalse(isreal(R));
R = [complex(zeros(0, 0)), true];
assert_isfalse(isreal(R));
R = [complex(single(zeros(0, 0))), 1];
assert_isequal(class(R), 'single');
assert_isfalse(isreal(R));
R = [complex(zeros(0, 0)), single(1)];
assert_isequal(R, single(1));
assert_istrue(isreal(R));
R = [single(zeros(0, 1)), complex(zeros(0, 0))];
assert_isequal(class(R), 'single');
assert_istrue(isreal(R));
R = [complex(zeros(0, 0)), int8(1)];
assert_isequal(R, int8(1));
R = ['a', complex(zeros(0, 0))];
assert_isequal(R, 'a');
%=============================================================================
id = 'Nelson:UnableToConvert';
fmt = _('The following error occurred converting from %s to %s:\n%s');
msgLogical = sprintf(fmt, 'logical', 'char', _('Conversion to char from logical is not possible.'));
msgComplex = sprintf(fmt, 'double', 'char', _('Complex values cannot be converted to chars'));
assert_checkerror('[''a'', true]', msgLogical, id);
assert_checkerror('[true, ''a'']', msgLogical, id);
assert_checkerror('[''a''; true]', msgLogical, id);
assert_checkerror('[''a'', logical([])]', msgLogical, id);
assert_checkerror('[logical([]), ''a'']', msgLogical, id);
assert_checkerror('[''a'', int8(1), true]', msgLogical, id);
assert_checkerror('[''a'', true, 1i]', msgLogical, id);
assert_checkerror('[''a'', 1i]', msgComplex, id);
assert_checkerror('[1i, ''a'']', msgComplex, id);
assert_checkerror('[''a'', 1i, true]', msgComplex, id);
assert_checkerror('['''', 1i]', msgComplex, id);
assert_checkerror('[''a'', single(1i)]', sprintf(fmt, 'single', 'char', _('Complex values cannot be converted to chars')), id);
%=============================================================================
% an empty complex operand next to an integer is dropped
R = [int8(1), complex([])];
assert_isequal(R, int8(1));
R = [int8([]), complex([])];
assert_isequal(R, int8([]));
R = [int8(1), complex(zeros(1, 0))];
assert_isequal(R, int8(1));
%=============================================================================
% complex integers are not supported: an empty operand does not change that
id = 'Nelson:concatenation:complexIntegerUnsupported';
msg = _('Complex integers are not supported: an integer cannot be concatenated with a value that has a nonzero imaginary part.');
assert_checkerror('[int8([]), 1i]', msg, id);
assert_checkerror('[1i, uint16([])]', msg, id);
assert_checkerror('[int8(1), 1i, []]', msg, id);
assert_checkerror('[int8([]); 1i]', msg, id);
%=============================================================================
