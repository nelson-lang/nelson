%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('missing'), 0);
asserts.isequal(nargout('missing'), 1);
%=============================================================================
R = missing();
asserts.isequal(class(R), 'missing');
asserts.isequal(size(R), [1 1]);
asserts.istrue(ismissing(R));
%=============================================================================
