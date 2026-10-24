%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A missing operand takes the class of the first operand that is not missing:
% a double or a single holds it as NaN and a string as <missing>, then the
% concatenation follows its usual rules; any other class cannot hold it.
%=============================================================================
id = 'Nelson:invalidConversion';
fmt = _('Conversion to %s from missing is not possible.');
assert_checkerror('[true, 1, missing]', sprintf(fmt, 'logical'), id);
assert_checkerror('[missing, true]', sprintf(fmt, 'logical'), id);
assert_checkerror('[true, missing, []]', sprintf(fmt, 'logical'), id);
assert_checkerror('[missing, true, "s"]', sprintf(fmt, 'logical'), id);
assert_checkerror('[missing, true, single(1)]', sprintf(fmt, 'logical'), id);
assert_checkerror('[repmat(missing, 1, 1, 2), true, []]', sprintf(fmt, 'logical'), id);
assert_checkerror('[int8(1), 1, missing]', sprintf(fmt, 'int8'), id);
assert_checkerror('[uint16(1), missing]', sprintf(fmt, 'uint16'), id);
assert_checkerror('[''a'', missing]', sprintf(fmt, 'char'), id);
assert_checkerror('[{1}, missing]', sprintf(fmt, 'cell'), id);
assert_checkerror('[missing, struct(''a'', 1), "s"]', sprintf(fmt, 'struct'), id);
assert_checkerror('[missing, @sin]', sprintf(fmt, 'function_handle'), id);
%=============================================================================
R = [1, true, missing];
assert_isequal(class(R), 'double');
assert_istrue(isequaln(R, [1 1 NaN]));
R = [missing, 1, true];
assert_istrue(isequaln(R, [NaN 1 1]));
R = [missing, 1, int8(1)];
assert_isequal(R, int8([0 1 1]));
R = [1, int8(1), missing];
assert_isequal(R, int8([1 1 0]));
R = [single(1), missing];
assert_isequal(class(R), 'single');
assert_istrue(isequaln(R, single([1 NaN])));
R = [1, 'a', missing];
assert_isequal(class(R), 'char');
assert_isequal(size(R), [1 3]);
R = [missing, [], 'a'];
assert_isequal(class(R), 'char');
assert_isequal(size(R), [1 2]);
R = [1, {1}, missing];
assert_isequal(class(R), 'cell');
assert_isequal(size(R), [1 3]);
R = [repmat(missing, 0, 0), [], struct('a', 1)];
assert_isequal(class(R), 'struct');
assert_isequal(size(R), [1 1]);
%=============================================================================
R = ["s", true, missing];
assert_isequal(class(R), 'string');
assert_isequal(ismissing(R), [false false true]);
R = [repmat(missing, 1, 2), "s"];
assert_isequal(ismissing(R), [true true false]);
R = [repmat(missing, 1, 1, 2), string.empty];
assert_isequal(size(R), [1 1 2]);
assert_istrue(all(ismissing(R)));
assert_isequal(ismissing(string(repmat(missing, 1, 2))), [true true]);
%=============================================================================
