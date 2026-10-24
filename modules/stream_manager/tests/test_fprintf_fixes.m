%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fprintf(fid) without format is an error (it used to read past the arguments)
msg = 'No format character vector.';
asserts.checkerror('fprintf(1)', msg, 'Nelson:stream_manager:noFormat');
asserts.checkerror('fprintf(1.0)', msg, 'Nelson:stream_manager:noFormat');
asserts.checkerror('fprintf(5)', msg, 'Nelson:stream_manager:noFormat');
%=============================================================================
