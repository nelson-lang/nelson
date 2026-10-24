%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('int32'), 1);
asserts.isequal(nargout('int32'), 1);
%=============================================================================
R = int32([-1 0 1]);
asserts.isequal(class(R), 'int32');
asserts.isequal(R, int32([-1 0 1]));
asserts.isequal(int32(double(intmax('int32')) * 2), intmax('int32'));
asserts.isequal(int32(double(intmin('int32')) * 2), intmin('int32'));
%=============================================================================
