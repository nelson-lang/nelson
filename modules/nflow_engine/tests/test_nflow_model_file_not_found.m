%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% A missing .nflow file (file-path API) and an unknown model (name API) have
% distinct identifiers.
%=============================================================================
f = [tempname(), '.nflow'];
asserts.checkerror('NFlow.exportfmu(f)', sprintf('The model file does not exist: %s', f), 'Nelson:nflow_engine:modelFileNotFound');
%=============================================================================
