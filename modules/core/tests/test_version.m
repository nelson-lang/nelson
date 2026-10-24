%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('version'), 1);
asserts.isequal(nargout('version'), 2);
%=============================================================================
v = version();
asserts.istrue(ischar(v));
asserts.istrue(~isempty(v));
n = version('-number');
asserts.istrue(isnumeric(n));
asserts.isequal(size(n), [1 4]);
c = version('-compiler');
asserts.istrue(iscell(c));
[v, r] = version();
asserts.istrue(ischar(v));
asserts.istrue(ischar(r));
%=============================================================================
