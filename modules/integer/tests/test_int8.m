%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('int8'), 1);
asserts.isequal(nargout('int8'), 1);
%=============================================================================
R = int8([-1 0 1]);
asserts.isequal(class(R), 'int8');
asserts.isequal(R, int8([-1 0 1]));
asserts.isequal(int8(200), intmax('int8'));
asserts.isequal(int8(-200), intmin('int8'));
%=============================================================================
