%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('inferiorto'), -1);
assert_isequal(nargout('inferiorto'), 0);
%=============================================================================
inferiorto('double');
inferiorto('double', 'int8');
inferiorto("single");
%=============================================================================
assert_checkerror('inferiorto(3)', _('Class names must be character vectors.'));
%=============================================================================
assert_checkerror('inferiorto()', _('Not enough input arguments.'));
%=============================================================================
