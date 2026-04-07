%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1550
% <-- Short Description -->
% getpid('available') did not work as expected.
%=============================================================================
%<--INTERACTIVE TEST-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
% <--ADV-CLI MODE-->
%=============================================================================
% start N Nelson sessions in parallel and check that getpid('available') returns an array of N PIDs.
%=============================================================================
