%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('FFTWwrapper'), 3);
asserts.isequal(nargout('FFTWwrapper'), 1);
%=============================================================================
asserts.istrue(FFTWwrapper('free'));
asserts.istrue(FFTWwrapper('load'));
%=============================================================================
