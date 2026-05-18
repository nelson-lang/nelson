%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
[n, values, first] = bytecode_varargin_varargout(10, 'abc', [1 2 3]);
assert_isequal(n, 3);
assert_isequal(values{1}, 10);
assert_isequal(values{2}, 'abc');
assert_isequal(values{3}, [1 2 3]);
assert_isequal(first, 10);
%=============================================================================
[~, values, first] = bytecode_varargin_varargout(42, 'kept');
assert_isequal(values, {42, 'kept'});
assert_isequal(first, 42);
%=============================================================================
[n, ~, first] = bytecode_varargin_varargout();
assert_isequal(n, 0);
assert_isequal(first, []);
%=============================================================================
