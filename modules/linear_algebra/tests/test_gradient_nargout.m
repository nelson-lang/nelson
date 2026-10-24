%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% calling gradient as a statement must not error (nargout == 0 still computes
% one output; previously the loop 1:min(nd,nargout) produced nothing).
gradient([1 2 4]);
%=============================================================================
% single output kept
r = gradient([1 2 4]);
asserts.isequal(r, [1 1.5 2]);
r = gradient(1:5, 2);
asserts.isequal(r, [0.5 0.5 0.5 0.5 0.5]);
%=============================================================================
