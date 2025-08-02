%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filesrc = [nelsonroot(), '/modules/json/tests/bench_jsonencode.json'];
r = fileread(filesrc, 'char');
tic();st = jsondecode(r);toc()
tic();txt =jsonencode(st);toc()
%=============================================================================
