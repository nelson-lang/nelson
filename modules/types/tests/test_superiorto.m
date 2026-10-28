%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('superiorto'), -1);
assert_isequal(nargout('superiorto'), 0);
%=============================================================================
superiorto('double');
superiorto('double', 'int8');
superiorto("single");
%=============================================================================
assert_checkerror('superiorto(3)', _('Class names must be character vectors.'));
%=============================================================================
assert_checkerror('superiorto()', _('Not enough input arguments.'));
%=============================================================================
