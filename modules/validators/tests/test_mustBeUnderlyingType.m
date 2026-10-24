%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeUnderlyingType'), 2);
asserts.isequal(nargout('mustBeUnderlyingType'), 0);
%=============================================================================
% passes silently when the underlying type matches
mustBeUnderlyingType(3, 'double');
mustBeUnderlyingType(int32(3), 'int32');
mustBeUnderlyingType(single([1 2 3]), 'single');
mustBeUnderlyingType('abc', 'char');
mustBeUnderlyingType(true, 'logical');
%=============================================================================
% errors on mismatch
asserts.checkerror('mustBeUnderlyingType(3, ''int32'')', sprintf(_('Value must be of underlying type ''%s''.'), 'int32'), 'Nelson:validators:mustBeUnderlyingType');
asserts.checkerror('mustBeUnderlyingType(int8(1), ''double'')', sprintf(_('Value must be of underlying type ''%s''.'), 'double'), 'Nelson:validators:mustBeUnderlyingType');
%=============================================================================
