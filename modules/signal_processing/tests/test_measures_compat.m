%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(rms([Inf 1]), Inf);
asserts.isequal(peak2peak([Inf 1 -Inf]), Inf);
%=============================================================================
asserts.checkerror('rms([1 2], ''bad'')', _('Unsupported option.'));
asserts.checkerror('peak2peak([1 2], ''bad'')', _('Unsupported option.'));
%=============================================================================
