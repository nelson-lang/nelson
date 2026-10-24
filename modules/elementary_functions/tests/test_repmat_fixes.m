%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('repmat(1:3,2.5)', _('Replication factors must be a row vector of integers or integer scalars.'));
asserts.checkerror('repmat(1:3,NaN)', _('Replication factors must be a row vector of integers or integer scalars.'));
asserts.checkerror('repmat(1:3,zeros(1,0))', _('Replication factors must be a row vector of integers or integer scalars.'));
asserts.checkerror('repmat(1:3,[1 NaN])', _('Replication factors must be a row vector of integers or integer scalars.'));
%=============================================================================
