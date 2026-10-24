%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('argv'), -1);
asserts.isequal(nargout('argv'), 1);
%=============================================================================
args = argv();
asserts.isequal(class(args), 'cell');
for k = 1:numel(args)
  asserts.isequal(class(args{k}), 'char');
end
%=============================================================================
user_args = argv('user');
asserts.isequal(class(user_args), 'cell');
for k = 1:numel(user_args)
  asserts.isequal(class(user_args{k}), 'char');
end
%=============================================================================
asserts.checkerror('argv(''bad'')', sprintf(_('Wrong value for argument #%d: %s expected.'), 1, '''user'''), 'Nelson:argv:InvalidValue');
asserts.checkerror('argv(1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'), 'Nelson:argv:InvalidArgument');
%=============================================================================
