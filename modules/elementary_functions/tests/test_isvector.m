%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isvector'), 1)
asserts.isequal(nargout('isvector'), 1)
%=============================================================================
asserts.isfalse(isvector([]));
asserts.isfalse(isvector(ones(0, 3)));
asserts.istrue(isvector('hello'));
asserts.isfalse(isvector(zeros(3, 1, 3)));
%=============================================================================
