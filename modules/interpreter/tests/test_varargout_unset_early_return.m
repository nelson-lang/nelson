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
% Regression: a function whose signature declares 'varargout' must not error
% when it returns through a code path that never assigns 'varargout', as long
% as the caller does not request more outputs than there are named ones.
% Nelson only requires 'varargout' to be a defined cell array when
% nargout > (number of named outputs). This was the fatal blocker for LTFAT
% startup (ltfatarghelper 'set' branch returns before filling varargout).
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
% nargout == 0 (statement form): early return with varargout unset must work.
varargout_early_return_function('early');
%=============================================================================
% nargout == 1 (== number of named outputs): must return the named output.
x = varargout_early_return_function('early');
asserts.isequal(x, 1);
%=============================================================================
% nargout > number of named outputs, varargout unset: must STILL error.
asserts.checkerror('[p, q] = varargout_early_return_function(''early'');', ...
  _('The special variable ''varargout'' was not defined as a cell-array.'));
%=============================================================================
% The path that does assign varargout keeps working for every nargout.
y = varargout_early_return_function('late');
asserts.isequal(y, 1);
[p, q] = varargout_early_return_function('late');
asserts.isequal(p, 1);
asserts.isequal(q, 2);
%=============================================================================
