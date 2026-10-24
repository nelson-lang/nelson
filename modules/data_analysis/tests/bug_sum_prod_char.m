%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% sum on char promotes to double (was "function char_sum undefined");
% prod on char errors with a message aligned with expected behavior
%=============================================================================
asserts.isequal(sum('ab'), 195);
asserts.isequal(class(sum('ab')), 'double');
asserts.isequal(sum('ab', 'double'), 195);
asserts.isequal(sum(['ab'; 'cd']), [196 198]);
asserts.isequal(sum(['ab'; 'cd'], 2), [195; 199]);
asserts.isequal(sum(''), 0);
%=============================================================================
asserts.checkerror('sum(''ab'', ''native'')', _('Native accumulation on char array is not supported.'));
%=============================================================================
asserts.checkerror('prod(''ab'')', _('PROD does not work with char inputs.'));
%=============================================================================
