%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% missing values concatenate into a missing array
m = [missing missing];
assert_isequal(class(m), 'missing');
assert_isequal(size(m), [1 2]);
assert_isequal(ismissing(m), [true true]);
m = [missing; missing];
assert_isequal(class(m), 'missing');
assert_isequal(size(m), [2 1]);
m = [missing missing; missing missing];
assert_isequal(size(m), [2 2]);
m = horzcat(missing, missing, missing);
assert_isequal(size(m), [1 3]);
m = vertcat(missing, missing);
assert_isequal(size(m), [2 1]);
m = cat(3, missing, missing);
assert_isequal(size(m), [1 1 2]);
m = [missing missing];
assert_isequal(class(m(2)), 'missing');
%=============================================================================
% with floating-point values missing becomes NaN
assert_isequal([missing missing 3], [NaN NaN 3]);
assert_isequal([missing, missing; 1 2], [NaN NaN; 1 2]);
assert_isequal([missing 1], [NaN 1]);
assert_isequal([missing; 2], [NaN; 2]);
assert_isequal([missing single(1)], single([NaN 1]));
%=============================================================================
% an empty operand still sets the class of the result
assert_isequal([missing []], NaN);
assert_isequal([[] missing], NaN);
assert_isequal([missing; []], NaN);
assert_isequal([missing zeros(1, 0)], NaN);
assert_isequal([missing; zeros(0, 2)], NaN);
assert_isequal([missing single([])], single(NaN));
assert_isequal([missing missing []], [NaN NaN]);
assert_isequal([missing [] 1], [NaN 1]);
assert_isequal(horzcat(missing, []), NaN);
assert_isequal(cat(1, missing, []), NaN);
R = [missing sparse([])];
assert_istrue(issparse(R));
assert_isequal(full(R), NaN);
R = [missing strings(0)];
assert_isequal(class(R), 'string');
assert_istrue(ismissing(R));
asserts.checkerror('[missing int8([])]', 'Conversion to int8 from missing is not possible.');
asserts.checkerror('[missing {}]', 'Conversion to cell from missing is not possible.');
asserts.checkerror('[missing '''']', 'Conversion to char from missing is not possible.');
%=============================================================================
% with strings missing becomes <missing>
R = [missing "a"];
assert_isequal(class(R), 'string');
assert_isequal(ismissing(R), [true false]);
%=============================================================================
% classes without a missing value reject it
asserts.checkerror('[missing int8(1)]', 'Conversion to int8 from missing is not possible.');
asserts.checkerror('[missing int8(1)]', 'Conversion to int8 from missing is not possible.', ...
  'Nelson:invalidConversion');
asserts.checkerror('[missing true]', 'Conversion to logical from missing is not possible.');
asserts.checkerror('[missing ''a'']', 'Conversion to char from missing is not possible.');
asserts.checkerror('[missing; ''a'']', 'Conversion to char from missing is not possible.');
asserts.checkerror('[missing {1}]', 'Conversion to cell from missing is not possible.');
asserts.checkerror('[{1} missing]', 'Conversion to cell from missing is not possible.');
asserts.checkerror('[missing struct(''a'', 1)]', 'Conversion to struct from missing is not possible.');
asserts.checkerror('[missing @sin]', 'Conversion to function_handle from missing is not possible.');
%=============================================================================
