%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% A call with nargout 0 returns varargout{1} (or the first named output) when
% it is assigned: the statement form sets and displays ans.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
addpath([nelsonroot(), '/modules/interpreter/tests/classdef/']);
%=============================================================================
clear('ans');
printed = evalc('varargout_only_function(1)');
asserts.istrue(contains(printed, 'ans ='));
asserts.istrue(contains(printed, '11'));
asserts.isequal(ans, 11);
%=============================================================================
clear('ans');
printed = evalc('varargout_early_return_function(''late'')');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, 1);
%=============================================================================
clear('ans');
printed = evalc('varargout_early_return_function(''early'')');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, 1);
%=============================================================================
% classdef overloading the size builtin with varargout
o = ClassdefVarargoutSize();
clear('ans');
printed = evalc('size(o)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, [3 4]);
asserts.isequal(size(o), [3 4]);
%=============================================================================
