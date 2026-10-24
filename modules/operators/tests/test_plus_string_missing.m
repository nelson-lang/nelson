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
% <missing> propagates through string concatenation with +
m = string(NaN);
R = m + " sec";
assert_isequal(class(R), 'string');
assert_istrue(ismissing(R));
R = "a" + m;
assert_istrue(ismissing(R));
R = m + m;
assert_istrue(ismissing(R));
R = m + 'abc';
assert_istrue(ismissing(R));
R = [m, "b"] + " sec";
assert_isequal(ismissing(R), [true false]);
assert_isequal(R(2), "b sec");
R = "1" + " sec";
assert_isequal(R, "1 sec");
%=============================================================================
