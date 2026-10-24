%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('feature'), 2);
asserts.isequal(nargout('feature'), 1);
%=============================================================================
initialValue = feature('currentAxesOnClick');
asserts.istrue(islogical(initialValue));
feature('currentAxesOnClick', false);
asserts.isfalse(feature('currentAxesOnClick'));
feature('currentAxesOnClick', initialValue);
asserts.isequal(feature('currentAxesOnClick'), initialValue);
%=============================================================================
