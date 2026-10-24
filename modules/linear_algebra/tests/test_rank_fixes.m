%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('rank(NaN)', _('Input matrix must not contain NaN or Inf values.'));
asserts.checkerror('rank([1 Inf])', _('Input matrix must not contain NaN or Inf values.'));
asserts.checkerror('rank(ones(2,2,2))', _('Input must be 2-dimensional.'));
asserts.checkerror('rank(magic(4),''a'')', _('Second input must be a real numeric tolerance.'));
asserts.checkerror('rank(magic(4),1i)', _('Second input must be a real numeric tolerance.'));
asserts.checkerror('rank(magic(4),true)', _('Second input must be a real numeric tolerance.'));
asserts.checkerror('rank([1 NaN])', _('Input matrix must not contain NaN or Inf values.'));
asserts.checkerror('rank([1 2;3 NaN])', _('Input matrix must not contain NaN or Inf values.'));
asserts.checkerror('rank(-Inf)', _('Input matrix must not contain NaN or Inf values.'));
asserts.checkerror('rank(single(NaN))', _('Input matrix must not contain NaN or Inf values.'));
asserts.checkerror('rank(magic(4),''x'')', _('Second input must be a real numeric tolerance.'));
%=============================================================================
