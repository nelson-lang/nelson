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
asserts.isequal(n, 3);
asserts.isequal(values{1}, 10);
asserts.isequal(values{2}, 'abc');
asserts.isequal(values{3}, [1 2 3]);
asserts.isequal(first, 10);
%=============================================================================
[~, values, first] = bytecode_varargin_varargout(42, 'kept');
asserts.isequal(values, {42, 'kept'});
asserts.isequal(first, 42);
%=============================================================================
[n, ~, first] = bytecode_varargin_varargout();
asserts.isequal(n, 0);
asserts.isequal(first, []);
%=============================================================================
