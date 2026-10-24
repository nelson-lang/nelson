%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A struct concatenation drops the empty operands of any class; the first
% other operand that is not a struct cannot be converted (three or more
% operands used to crash).
%=============================================================================
id = 'Nelson:UnableToConvert';
fmt = _('The following error occurred converting from %s to %s:\n%s');
tail = _('Conversion to struct from %s is not possible.');
msg = @(c) sprintf(fmt, c, 'struct', sprintf(tail, c));
assert_checkerror('[1, struct(''a'', 1)]', msg('double'), id);
assert_checkerror('[struct(''a'', 1), 1]', msg('double'), id);
assert_checkerror('[1, 1, struct(''a'', 1)]', msg('double'), id);
assert_checkerror('[1; 2; struct(''a'', 1)]', msg('double'), id);
assert_checkerror('[struct(''a'', 1), 1, 1]', msg('double'), id);
assert_checkerror('[[], 1, struct(''a'', 1)]', msg('double'), id);
assert_checkerror('[struct(''a'', 1), [], 1]', msg('double'), id);
assert_checkerror('[struct(''a'', 1), 1, struct(''b'', 1)]', msg('double'), id);
assert_checkerror('[1i, struct(''a'', 1)]', msg('double'), id);
assert_checkerror('[zeros(1, 1, 2), struct(''a'', 1)]', msg('double'), id);
assert_checkerror('[struct(''a'', 1), ones(2, 2)]', msg('double'), id);
assert_checkerror('[struct(''a'', 1), ''a'']', msg('char'), id);
assert_checkerror('[true, struct(''a'', 1)]', msg('logical'), id);
assert_checkerror('[int8(1), struct(''a'', 1)]', msg('int8'), id);
assert_checkerror('[single(1), struct(''a'', 1)]', msg('single'), id);
assert_checkerror('[missing, 1, struct(''a'', 1)]', msg('double'), id);
%=============================================================================
R = [zeros(0, 1), struct('a', 1)];
assert_isequal(R, struct('a', 1));
R = [struct('a', 1), zeros(0, 1)];
assert_isequal(R, struct('a', 1));
R = ['', struct('a', 1)];
assert_isequal(R, struct('a', 1));
R = [[], [], struct('a', 1)];
assert_isequal(R, struct('a', 1));
%=============================================================================
