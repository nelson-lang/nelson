%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('str2func'), 1);
assert_isequal(nargout('str2func'), 1);
%=============================================================================
FH = str2func('run');
assert_checkerror('FHS = [FH; FH; FH];', _('Nonscalar arrays of function handles are not allowed; use cell arrays instead.'));
%=============================================================================
FHS = {FH; FH; FH};
%=============================================================================
assert_isequal(size(FH), [1 1]);
assert_isequal(size(FHS), [3 1]);
%=============================================================================
assert_isequal(length(FH), 1);
assert_isequal(length(FHS), 3);
%=============================================================================
assert_isequal(length(FH), 1);
assert_isequal(length(FHS), 3);
%=============================================================================
assert_isequal(ndims(FH), 2);
assert_isequal(ndims(FHS), 2);
%=============================================================================
