%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('namelengthmax'), 1);
asserts.isequal(nargout('namelengthmax'), 1);
%=============================================================================
r = namelengthmax();
asserts.istrue(isnumeric(r));
asserts.istrue(isscalar(r));
asserts.istrue(r > 0);
%=============================================================================
