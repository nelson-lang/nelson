%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('str2func'), 1);
asserts.isequal(nargout('str2func'), 1);
%=============================================================================
FH = str2func('run');
asserts.checkerror('FHS = [FH; FH; FH];', _('Nonscalar arrays of function handles are not allowed; use cell arrays instead.'));
%=============================================================================
FHS = {FH; FH; FH};
%=============================================================================
asserts.isequal(size(FH), [1 1]);
asserts.isequal(size(FHS), [3 1]);
%=============================================================================
asserts.isequal(length(FH), 1);
asserts.isequal(length(FHS), 3);
%=============================================================================
asserts.isequal(length(FH), 1);
asserts.isequal(length(FHS), 3);
%=============================================================================
asserts.isequal(ndims(FH), 2);
asserts.isequal(ndims(FHS), 2);
%=============================================================================
