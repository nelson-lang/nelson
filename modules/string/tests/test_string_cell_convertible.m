%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Each element of a cell converted to a string array must become one string:
% a scalar, a character row ('' too) or a 1x1 cell holding such a value. The
% first other element, by linear index, names the error.
%=============================================================================
id = 'Nelson:string:MustBeConvertibleCellArray';
fmt = _('Conversion from cell failed. Element %d must be convertible to a string scalar.');
msg1 = sprintf(fmt, 1);
msg2 = sprintf(fmt, 2);
%=============================================================================
assert_checkerror('string({@sin})', msg1, id);
assert_checkerror('string({struct(''a'', 1)})', msg1, id);
assert_checkerror('string({[1 2]})', msg1, id);
assert_checkerror('string({[]})', msg1, id);
assert_checkerror('string({zeros(0, 1)})', msg1, id);
assert_checkerror('string({{}})', msg1, id);
assert_checkerror('string({{[1 2]}})', msg1, id);
assert_checkerror('string({sparse(1)})', msg1, id);
assert_checkerror('string({int8([1 2])})', msg1, id);
assert_checkerror('string({[''ab''; ''cd'']})', msg1, id);
assert_checkerror('string({true(1, 2)})', msg1, id);
assert_checkerror('string({["x" "y"]})', msg1, id);
assert_checkerror('string({string.empty})', msg1, id);
assert_checkerror('string({char(zeros(0, 3))})', msg1, id);
assert_checkerror('string({function_handle.empty})', msg1, id);
assert_checkerror('string({struct([])})', msg1, id);
assert_checkerror('string({1, @sin})', msg2, id);
assert_checkerror('string({1; []})', msg2, id);
assert_checkerror('string({1, {1, 2}})', msg2, id);
assert_checkerror('string({1, [1 2], @sin})', msg2, id);
assert_checkerror('string({1, 2; [], 4})', msg2, id);
assert_checkerror('string({1, 2; 3, []})', sprintf(fmt, 4), id);
%=============================================================================
assert_checkerror('["a", {@sin}]', msg1, id);
assert_checkerror('[{struct(''a'', 1)}, "a"]', msg1, id);
assert_checkerror('["a"; {[1 2]}]', msg1, id);
assert_checkerror('["a", {1, []}]', msg2, id);
assert_checkerror('["a", {1; {}}]', msg2, id);
%=============================================================================
assert_isequal(string({{1}}), "1");
assert_isequal(string({{{1}}}), "1");
assert_isequal(string({{'x'}}), "x");
assert_isequal(string({'ab'}), "ab");
assert_isequal(string({''}), "");
assert_isequal(string({"x"}), "x");
assert_isequal(string({int8(5)}), "5");
assert_isequal(string({single(2.5)}), "2.5");
assert_isequal(string({1+2i}), "1+2i");
assert_istrue(ismissing(string({NaN})));
assert_istrue(ismissing(string({missing})));
assert_isequal(["a", {1, {1}}], ["a", "1", "1"]);
assert_isequal(["a", {1, 'ab'}], ["a", "1", "ab"]);
%=============================================================================
