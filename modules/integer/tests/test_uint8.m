%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('uint8'), 1);
asserts.isequal(nargout('uint8'), 1);
%=============================================================================
R = uint8([-1 0 1]);
asserts.isequal(class(R), 'uint8');
asserts.isequal(R, uint8([0 0 1]));
asserts.isequal(uint8(300), intmax('uint8'));
asserts.isequal(uint8(-1), intmin('uint8'));
%=============================================================================
