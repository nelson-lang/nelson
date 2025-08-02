%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('@libpointer/fieldnames'), 1);
assert_isequal(nargout('@libpointer/fieldnames'), 1);
%=============================================================================
a = libpointer('doublePtr', eye(2, 2));
c = fieldnames(a);
ref = {'Value';'DataType'};
assert_isequal(c, ref);
%=============================================================================
