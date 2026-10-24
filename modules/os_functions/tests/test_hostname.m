%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('hostname'), 0);
asserts.isequal(nargout('hostname'), 1);
%=============================================================================
R = hostname();
asserts.istrue(ischar(R));
asserts.istrue(isvector(R));
asserts.isfalse(isempty(R));
%=============================================================================
